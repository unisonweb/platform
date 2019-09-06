{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Unison.FileParsers where

import Unison.Prelude

import Control.Lens (view, _3)
import qualified Unison.Parser as Parser
import           Control.Monad.State        (evalStateT)
import Control.Monad.Writer (tell)
import           Data.Bifunctor             ( first )
import qualified Data.Foldable              as Foldable
import qualified Data.Map                   as Map
import Data.List (partition)
import qualified Data.Set                   as Set
import qualified Data.Sequence              as Seq
import           Data.Text                  (unpack)
import qualified Unison.ABT                 as ABT
import qualified Unison.Blank               as Blank
import qualified Unison.Name                as Name
import qualified Unison.Names3              as Names
import           Unison.Parser              (Ann)
import qualified Unison.Parsers             as Parsers
import qualified Unison.Referent            as Referent
import           Unison.Reference           (Reference)
import           Unison.Result              (Note (..), Result, pattern Result, ResultT)
import qualified Unison.Result              as Result
import           Unison.Term                (AnnotatedTerm)
import qualified Unison.Term                as Term
import qualified Unison.Type
import qualified Unison.Typechecker         as Typechecker
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.Typechecker.Context as Context
import qualified Unison.UnisonFile          as UF
import qualified Unison.Util.List           as List
import qualified Unison.Util.Relation       as Rel
import           Unison.Var                 (Var)
import qualified Unison.Var                 as Var
import Unison.Names3 (Names0)

type Term v = AnnotatedTerm v Ann
type Type v = Unison.Type.Type v Ann
type UnisonFile v = UF.UnisonFile v Ann
type Result' v = Result (Seq (Note v Ann))

convertNotes :: Ord v => Typechecker.Notes v ann -> Seq (Note v ann)
convertNotes (Typechecker.Notes es is) =
  (TypeError <$> es) <> (TypeInfo <$> Seq.fromList is') where
  is' = snd <$> List.uniqueBy' f ([(1::Word)..] `zip` Foldable.toList is)
  f (_, Context.TopLevelComponent cs) = Right [ v | (v,_,_) <- cs ]
  f (i, _) = Left i
  -- each round of TDNR emits its own TopLevelComponent notes, so we remove
  -- duplicates (based on var name and location), preferring the later note as
  -- that will have the latest typechecking info

parseAndSynthesizeFile
  :: (Var v, Monad m)
  => [Type v]
  -> (Set Reference -> m (TL.TypeLookup v Ann))
  -> Parser.ParsingEnv
  -> FilePath
  -> Text
  -> ResultT
       (Seq (Note v Ann))
       m
       (Either Names0 (UF.TypecheckedUnisonFile v Ann))
parseAndSynthesizeFile ambient typeLookupf env filePath src = do
  uf <- Result.fromParsing $ Parsers.parseFile filePath (unpack src) env
  let names0 = Names.currentNames (Parser.names env)
  (tm, tdnrMap, typeLookup) <- resolveNames typeLookupf names0 uf
  let (Result notes' r) = synthesizeFile ambient typeLookup tdnrMap uf tm
  tell notes' $> maybe (Left (UF.toNames uf )) Right r

type TDNRMap v = Map Typechecker.Name [Typechecker.NamedReference v Ann]

resolveNames
  :: (Var v, Monad m)
  => (Set Reference -> m (TL.TypeLookup v Ann))
  -> Names.Names0
  -> UnisonFile v
  -> ResultT
       (Seq (Note v Ann))
       m
       (AnnotatedTerm v Ann, TDNRMap v, TL.TypeLookup v Ann)
resolveNames typeLookupf preexistingNames uf = do
  let tm = UF.typecheckingTerm uf
      deps = Term.dependencies tm
      possibleDeps = [ (Name.toText name, Var.name v, r) |
        (name, r) <- Rel.toList (Names.terms0 preexistingNames),
        v <- Set.toList (Term.freeVars tm),
        Name.unqualified name == Name.unqualified (Name.fromVar v) ]
      possibleRefs = Referent.toReference . view _3 <$> possibleDeps
  tl <- lift . lift . fmap (UF.declsToTypeLookup uf <>)
      $ typeLookupf (deps <> Set.fromList possibleRefs)
  let fqnsByShortName = List.multimap $
        [ (shortname, nr) |
          (name, shortname, r) <- possibleDeps,
          typ <- toList $ TL.typeOfReferent tl r,
          let nr = Typechecker.NamedReference name typ (Right r) ] <>
        [ (shortname, nr) |
          (name, r) <- Rel.toList (Names.terms0 $ UF.toNames uf),
          typ <- toList $ TL.typeOfReferent tl r,
          let shortname = Name.toText $ Name.unqualified name,
          let nr = Typechecker.NamedReference (Name.toText name) typ (Right r) ]
  pure (tm, fqnsByShortName, tl)

synthesizeFile
  :: forall v
   . Var v
  => [Type v]
  -> TL.TypeLookup v Ann
  -> TDNRMap v
  -> UnisonFile v
  -> AnnotatedTerm v Ann
  -> Result (Seq (Note v Ann)) (UF.TypecheckedUnisonFile v Ann)
synthesizeFile ambient tl fqnsByShortName uf term = do
  let -- substitute Blanks for any remaining free vars in UF body
    tdnrTerm = Term.prepareTDNR term
    env0 = Typechecker.Env ambient tl fqnsByShortName
    Result notes mayType =
      evalStateT (Typechecker.synthesizeAndResolve env0) tdnrTerm
  -- If typechecking succeeded, reapply the TDNR decisions to user's term:
  Result (convertNotes notes) mayType >>= \_typ -> do
    let infos = Foldable.toList $ Typechecker.infos notes
    (topLevelComponents :: [[(v, Term v, Type v)]]) <-
      let
        topLevelBindings :: Map v (Term v)
        topLevelBindings = Map.mapKeys Var.reset $ extractTopLevelBindings tdnrTerm
        extractTopLevelBindings (Term.LetRecNamedAnnotatedTop' True _ bs body) =
          Map.fromList (first snd <$> bs) <> extractTopLevelBindings body
        extractTopLevelBindings _                        = Map.empty
        tlcsFromTypechecker =
          List.uniqueBy' (fmap vars)
            [ t | Context.TopLevelComponent t <- infos ]
          where vars (v, _, _) = v
        strippedTopLevelBinding (v, typ, redundant) = do
          tm <- case Map.lookup v topLevelBindings of
            Nothing ->
              Result.compilerBug $ Result.TopLevelComponentNotFound v term
            Just (Term.Ann' x _) | redundant -> pure x
            Just x                           -> pure x
          -- The Var.reset removes any freshening added during typechecking
          pure (Var.reset v, tm, typ)
      in
        -- use tlcsFromTypechecker to inform annotation-stripping decisions
        traverse (traverse strippedTopLevelBinding) tlcsFromTypechecker
    let doTdnr = applyTdnrDecisions infos
        doTdnrInComponent (v, t, tp) = (\t -> (v, t, tp)) <$> doTdnr t
    _          <- doTdnr tdnrTerm
    tdnredTlcs <- (traverse . traverse) doTdnrInComponent topLevelComponents
    let (watches', terms') = partition isWatch tdnredTlcs
        isWatch = all (\(v,_,_) -> Set.member v watchedVars)
        watchedVars = Set.fromList [ v | (v, _) <- UF.allWatches uf ]
        tlcKind [] = error "empty TLC, should never occur"
        tlcKind tlc@((v,_,_):_) = let
          hasE k =
            elem v . fmap fst $ Map.findWithDefault [] k (UF.watches uf)
          in case Foldable.find hasE (Map.keys $ UF.watches uf) of
               Nothing -> error "wat"
               Just kind -> (kind, tlc)
    pure $ UF.typecheckedUnisonFile
             (UF.dataDeclarations uf)
             (UF.effectDeclarations uf)
             terms'
             (map tlcKind watches')
 where
  applyTdnrDecisions
    :: [Context.InfoNote v Ann]
    -> Term v
    -> Result' v (Term v)
  applyTdnrDecisions infos tdnrTerm = foldM go tdnrTerm decisions
   where
    -- UF data/effect ctors + builtins + TLC Term.vars
    go term _decision@(shortv, loc, replacement) =
      ABT.visit (resolve shortv loc replacement) term
    decisions =
      [ (v, loc, replacement) | Context.Decision v loc replacement <- infos ]
    -- resolve (v,loc) in a matching Blank to whatever `fqn` maps to in `names`
    resolve shortv loc replacement t = case t of
      Term.Blank' (Blank.Recorded (Blank.Resolve loc' name))
        | loc' == loc && Var.nameStr shortv == name ->
          -- loc of replacement already chosen correctly by whatever made the
          -- Decision
          pure . pure $ replacement
      _ -> Nothing
