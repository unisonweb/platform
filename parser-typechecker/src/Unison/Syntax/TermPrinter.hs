module Unison.Syntax.TermPrinter
  ( emptyAc,
    pretty,
    prettyBlock,
    prettyBlock',
    pretty',
    prettyBinding,
    prettyBinding',
    prettyBindingWithoutTypeSignature,
    prettyDoc2,
    pretty0,
    runPretty,
    prettyPattern,
  )
where

import Control.Lens (unsnoc)
import Control.Monad.Reader (ask, local)
import Control.Monad.State (evalState)
import Control.Monad.State qualified as State
import Data.Char (isPrint)
import Data.Foldable qualified as Foldable
import Data.List
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (unpack)
import Data.Text qualified as Text
import Data.Vector ()
import Text.Show.Unicode qualified as U
import Unison.ABT (annotation, reannotateUp, pattern AbsN')
import Unison.ABT qualified as ABT
import Unison.Blank qualified as Blank
import Unison.Builtin.Decls (pattern TuplePattern, pattern TupleTerm')
import Unison.Builtin.Decls qualified as DD
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorReference qualified as ConstructorReference
import Unison.ConstructorType qualified as CT
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Pattern (Pattern)
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnv qualified as PrettyPrintEnv
import Unison.PrettyPrintEnv.FQN (Imports, Prefix, Suffix, elideFQN)
import Unison.PrettyPrintEnv.MonadPretty
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Syntax.HashQualified qualified as HQ (unsafeFromVar)
import Unison.Syntax.Lexer.Unison (showEscapeChar)
import Unison.Syntax.Name qualified as Name (isSymboly, parseText, parseTextEither, toText, unsafeParseText, unsafeParseVar)
import Unison.Syntax.NamePrinter (styleHashQualified'')
import Unison.Syntax.NameSegment qualified as NameSegment (toEscapedText)
import Unison.Syntax.Precedence (InfixPrecedence (..), Precedence (..), increment, isTopLevelPrecedence, operatorPrecedence)
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Term
import Unison.Type (Type, pattern ForallsNamed')
import Unison.Type qualified as Type
import Unison.Util.Bytes qualified as Bytes
import Unison.Util.Monoid (foldMapM, intercalateMap, intercalateMapM)
import Unison.Util.Pretty (ColorText, Pretty, Width)
import Unison.Util.Pretty qualified as PP
import Unison.Util.SyntaxText qualified as S
import Unison.Var (Var)
import Unison.Var qualified as Var

type SyntaxText = S.SyntaxText' Reference

-- Gets rid of unsightly "_eta" expansion in the pretty-printed output
etaReduce :: (Var v) => Term3 v a -> Term3 v a
etaReduce (LamNamed' v (App' f (Var' v'))) | v == v' && Var.name v == "_eta" = f
etaReduce tm = tm

goPretty :: (Var v) => PrettyPrintEnv -> Term2 v at ap v a -> Pretty SyntaxText
goPretty ppe tm = runPretty (avoidShadowing tm ppe) $ pretty0 emptyAc $ printAnnotate ppe tm

pretty :: (HasCallStack, Var v) => PrettyPrintEnv -> Term v a -> Pretty ColorText
pretty ppe tm =
  PP.syntaxToColor $ goPretty ppe tm

prettyBlock :: (Var v) => Bool -> PrettyPrintEnv -> Term v a -> Pretty ColorText
prettyBlock elideUnit ppe = PP.syntaxToColor . prettyBlock' elideUnit ppe

prettyBlock' :: (HasCallStack, Var v) => Bool -> PrettyPrintEnv -> Term v a -> Pretty SyntaxText
prettyBlock' elideUnit ppe tm =
  runPretty (avoidShadowing tm ppe) . pretty0 (emptyBlockAc {elideUnit = elideUnit}) $ printAnnotate ppe tm

pretty' :: (HasCallStack, Var v) => Maybe Width -> PrettyPrintEnv -> Term v a -> ColorText
pretty' (Just width) n t =
  PP.render width . PP.syntaxToColor $ goPretty n t
pretty' Nothing n t =
  PP.renderUnbroken . PP.syntaxToColor $ goPretty n t

-- Information about the context in which a term appears, which affects how the
-- term should be rendered.
data AmbientContext = AmbientContext
  { -- The operator precedence of the enclosing context (a number from 0 to 11,
    -- or -1 to render without outer parentheses unconditionally).
    -- Function application has precedence 10.
    precedence :: !Precedence,
    blockContext :: !BlockContext,
    infixContext :: !InfixContext,
    imports :: !Imports,
    docContext :: !DocLiteralContext,
    -- `True` if a `()` at the end of a block should be elided
    elideUnit :: !Bool
  }

-- Description of the position of this ABT node, when viewed in the
-- surface syntax.
data BlockContext
  = -- This ABT node is at the top level of a TermParser.block.
    Block
  | Normal
  deriving (Eq, Show)

data InfixContext
  = -- This ABT node is an infix operator being used in infix position.
    Infix
  | NonInfix
  deriving (Eq, Show)

data DocLiteralContext
  = -- We won't try and render this ABT node or anything under it as a [: @Doc literal :]
    NoDoc
  | -- We'll keep checking as we recurse down
    MaybeDoc
  deriving (Eq, Show)

{- Explanation of precedence handling

   We illustrate precedence rules as follows.

     >=Application
       (Application)f (Application)x

   This example shows that a function application f x is enclosed in
   parentheses whenever the ambient precedence around it is >= Application, and that
   when printing its two components, an ambient precedence of Application is used in
   both places.

   The pretty-printer uses the following rules for printing terms.

     >=Top
       let x = (Bottom)y
           (Statement)z

     >=Prefix
       ! (Prefix)x
       ' (Prefix)x
       (Prefix)x ?

     >=(Application)
       (Application)f (Application)x (Application)y ...
       termLink t
       typeLink t

     >=(Infix +)
       (Infix +)x + (Infix +)y + ... (Infix +)z

     Printing an infix operator in infix position has the following additional
     rule: If the operator has a lower precedence than the ambient precedence,
     it is enclosed in parentheses. If the operator has no precedence rule,
     its precedence is assumed to be higher than any operator to its right, and
     lower than any operator to its left.

     >(Control)
       x -> (Control)y

     >=(Control)
       if (Annotation)a then (Annotation)b else (Annotation)c
       handle (Annoration)b with (Annotation)h
       case (Control)x of
         a | (Control)g -> (Control)b

     >=(Annotation)
       (Application)a : (Annotation)Int

   And the following for patterns.

     >=Prefix
       x@(Prefix)p

     >=Application
       Con (Application)p (Application)q ...

     -- never any external parens added around the following
       { p }
       { Eff 10p 10q ... -> 0k }

-}

isBindingSoftHangable :: (Var v) => Term2 v at ap v a -> Bool
isBindingSoftHangable (isSoftHangable -> True) = True
isBindingSoftHangable (Apps' _ (unsnoc -> Just (_, last))) = isSoftHangable last
isBindingSoftHangable _ = False

pretty0 ::
  forall v m.
  (MonadPretty v m) =>
  AmbientContext ->
  Term3 v PrintAnnotation ->
  m (Pretty SyntaxText)
pretty0 a tm | isTopLevelPrecedence (precedence a) && not (isBindingSoftHangable tm) = do
  -- we allow use clause insertion here even when it otherwise wouldn't be
  -- (as long as the tm isn't soft hangable, if it gets soft hung then
  -- adding use clauses beforehand will mess things up)
  tmp <- pretty0 (a {imports = im, precedence = Bottom}) tm
  pure $ PP.lines (uses <> [tmp])
  where
    (im, uses) = calcImports (imports a) tm
pretty0
  a@AmbientContext
    { precedence = p,
      blockContext = bc,
      infixContext = ic,
      imports = im,
      docContext = doc
    }
  tm =
    let term = etaReduce tm
     in specialCases term \case
          Var' v -> do
            env <- ask
            let name =
                  if Set.member v env.freeTerms && Set.member v env.boundTerms
                    then HQ.fromName (Name.makeAbsolute (Name.unsafeParseVar v))
                    else elideFQN im $ HQ.unsafeFromVar (Var.reset v)
            pure . parenIfInfix name ic $ styleHashQualified'' (fmt S.Var) name
          Ref' r -> do
            env <- ask
            let name = elideFQN im $ PrettyPrintEnv.termName env.ppe (Referent.Ref r)
            pure . parenIfInfix name ic $ styleHashQualified'' (fmt $ S.TermReference (Referent.Ref r)) name
          TermLink' r -> do
            env <- ask
            let name = elideFQN im $ PrettyPrintEnv.termName env.ppe r
            pure . paren (p >= Application) $
              fmt S.LinkKeyword "termLink "
                <> parenIfInfix name ic (styleHashQualified'' (fmt $ S.TermReference r) name)
          TypeLink' r -> do
            env <- ask
            let name = elideFQN im $ PrettyPrintEnv.typeName env.ppe r
            pure . paren (p >= Application) $
              fmt S.LinkKeyword "typeLink "
                <> parenIfInfix name ic (styleHashQualified'' (fmt $ S.TypeReference r) name)
          Ann' tm t -> do
            tm' <- pretty0 (ac Application Normal im doc) tm
            tp' <- TypePrinter.pretty0 im 0 t
            pure . paren (p >= Annotation) $ tm' <> PP.hang (fmt S.TypeAscriptionColon " :") tp'
          Int' i -> pure . fmt S.NumericLiteral . l $ (if i >= 0 then ("+" ++ show i) else (show i))
          Nat' u -> pure . fmt S.NumericLiteral . l $ show u
          Float' f -> pure . fmt S.NumericLiteral . l $ show f
          -- TODO How to handle Infinity, -Infinity and NaN?  Parser cannot parse
          --      them.  Haskell doesn't have literals for them either.  Is this
          --      function only required to operate on terms produced by the parser?
          --      In which case the code is fine as it stands.  If it can somehow run
          --      on values produced by execution (or, one day, on terms produced by
          --      metaprograms), then it needs to be able to print them (and then the
          --      parser ought to be able to parse them, to maintain symmetry.)
          Boolean' b -> pure . fmt S.BooleanLiteral $ if b then l "true" else l "false"
          Text' s
            | Just quotes <- useRaw s ->
                pure . fmt S.TextLiteral $ PP.text quotes <> "\n" <> PP.text s <> "\n" <> PP.text quotes
            where
              -- we only use this syntax if we're not wrapped in something else,
              -- to avoid possible round trip issues if the text ends at an odd column
              useRaw _ | p >= Annotation = Nothing
              useRaw s | Text.elem '\n' s && Text.all ok s = Just quotes
              useRaw _ = Nothing
              ok ch = isPrint ch || ch == '\n' || ch == '\r'
              -- Picks smallest number of surrounding """ to be unique
              quotes = Text.pack (replicate numQuotes '"')
              numQuotes = max 3 $ longestRun '"' s + 1
              longestRun :: Char -> Text -> Int
              longestRun c = maximum . (0 :) . map Text.length . filter ((== c) . Text.head) . Text.group

          Text' s -> pure . fmt S.TextLiteral $ l $ U.ushow s
          Char' c -> pure
            . fmt S.CharLiteral
            . l
            $ case showEscapeChar c of
              Just c -> "?\\" ++ [c]
              Nothing -> '?' : [c]
          Blank' id -> pure $ fmt S.Blank $ l "_" <> l (fromMaybe "" (Blank.nameb id))
          Constructor' ref -> do
            env <- ask
            let name = elideFQN im $ PrettyPrintEnv.termName env.ppe conRef
                conRef = Referent.Con ref CT.Data
            pure $ styleHashQualified'' (fmt $ S.TermReference conRef) name
          Request' ref -> do
            env <- ask
            let name = elideFQN im $ PrettyPrintEnv.termName env.ppe conRef
                conRef = Referent.Con ref CT.Effect
            pure $ styleHashQualified'' (fmt $ S.TermReference conRef) name
          Handle' h body -> do
            pb <- pretty0 (ac Annotation Block im doc) body
            ph <- pretty0 (ac Annotation Block im doc) h
            let hangHandler = case h of
                  -- handle ... with cases
                  LamsNamedMatch' [] _ -> \a b -> a <> " " <> b
                  _ -> PP.hang
            pure . paren (p >= Control) $
              if PP.isMultiLine pb || PP.isMultiLine ph
                then
                  PP.lines
                    [ fmt S.ControlKeyword "handle" `PP.hang` pb,
                      fmt S.ControlKeyword "with" `hangHandler` ph
                    ]
                else
                  PP.spaced
                    [ fmt S.ControlKeyword "handle"
                        `PP.hang` pb
                        <> PP.softbreak
                        <> fmt S.ControlKeyword "with"
                          `hangHandler` ph
                    ]
          Delay' x
            | Match' _ _ <- x -> do
                px <- pretty0 (ac Annotation Block im doc) x
                let hang = if isSoftHangable x then PP.softHang else PP.hang
                pure . paren (p > Control) $
                  fmt S.ControlKeyword "do" `hang` px
            | otherwise -> do
                let (im0', uses0) = calcImports im x
                let allowUses = isLet x || (p == Bottom)
                let im' = if allowUses then im0' else im
                let uses = if allowUses then uses0 else []
                let soft = isSoftHangable x && null uses && p < Annotation
                let hang = if soft then PP.softHang else PP.hang
                px <- pretty0 (ac Annotation Block im' doc) x
                -- this makes sure we get proper indentation if `px` spills onto
                -- multiple lines, since `do` introduces layout block
                let indent = PP.Width (if soft then 2 else 0) + (if soft && p < Application then 1 else 0)
                pure . paren (p > Control) $
                  fmt S.ControlKeyword "do" `hang` PP.lines (uses <> [PP.indentNAfterNewline indent px])
          List' xs -> do
            let listLink p = fmt (S.TypeReference Type.listRef) p
            let comma = listLink ", " `PP.orElse` ("\n" <> listLink ", ")
            pelems <- traverse (fmap (PP.indentNAfterNewline 2) . pretty0 (ac Annotation Normal im doc)) xs
            let open = listLink "[" `PP.orElse` listLink "[ "
            let close = listLink "]" `PP.orElse` ("\n" <> listLink "]")
            pure $ PP.group (open <> PP.sep comma pelems <> close)
          If' cond t f ->
            do
              pcond <- pretty0 (ac Control Block im doc) cond
              pt <- pretty0 (ac Annotation Block im doc) t
              pf <- pretty0 (ac Annotation Block im doc) f
              pure . paren (p >= Control) $
                if PP.isMultiLine pcond
                  then
                    PP.lines
                      [ fmt S.ControlKeyword "if" `PP.hang` pcond,
                        fmt S.ControlKeyword "then" `PP.hang` pt,
                        fmt S.ControlKeyword "else" `PP.hang` pf
                      ]
                  else
                    if PP.isMultiLine pt || PP.isMultiLine pf
                      then
                        PP.lines
                          [ fmt S.ControlKeyword "if " <> pcond <> fmt S.ControlKeyword " then" `PP.hang` pt,
                            fmt S.ControlKeyword "else" `PP.hang` pf
                          ]
                      else
                        PP.spaced
                          [ (fmt S.ControlKeyword "if" `PP.hang` pcond) <> (fmt S.ControlKeyword " then" `PP.hang` pt),
                            fmt S.ControlKeyword "else" `PP.hang` pf
                          ]
          LetBlock bs e ->
            let (im', uses) = calcImports im term
             in printLet a {imports = im'} bc bs e uses
          -- Some matches are rendered as a destructuring bind, like
          --   match foo with (a,b) -> blah
          -- becomes
          --   (a,b) = foo
          --   blah
          -- See `isDestructuringBind` definition.
          Match' scrutinee cs@[MatchCase pat guard (AbsN' vs body)]
            | p <= Control && isDestructuringBind scrutinee cs -> do
                env <- ask
                let letIntro = case bc of
                      Block -> id
                      Normal -> \x -> fmt S.ControlKeyword "let" `PP.hang` x
                lhs <- do
                  let (lhs, _) = prettyPattern env.ppe (ac Annotation Block im doc) Application vs pat
                  guard' <- printGuard guard
                  pure $ PP.group lhs `PP.hang` guard'
                let eq = fmt S.BindingEquals "="
                rhs <- pretty0 (ac Bottom Block im doc) scrutinee
                letIntro <$> do
                  prettyBody <- pretty0 (ac Bottom Block im doc) body
                  pure $
                    PP.lines
                      [ (lhs <> eq) `PP.hang` rhs,
                        prettyBody
                      ]
            where
              printGuard Nothing = pure mempty
              printGuard (Just g') = do
                let (_, g) = ABT.unabs g'
                prettyg <- pretty0 (ac Control Normal im doc) g
                pure $ fmt S.DelimiterChar "| " <> prettyg
          Match' scrutinee branches ->
            do
              ps <- pretty0 (ac Control Normal im doc) scrutinee
              pbs <- printCase im doc (arity1Branches branches) -- don't print with `cases` syntax
              pure . paren (p >= Control) $
                if PP.isMultiLine ps
                  then
                    PP.lines
                      [ fmt S.ControlKeyword "match " `PP.hang` ps,
                        fmt S.ControlKeyword " with" `PP.hang` pbs
                      ]
                  else (fmt S.ControlKeyword "match " <> ps <> fmt S.ControlKeyword " with") `PP.hang` pbs
          Apps' f args -> paren (p >= Application) <$> (PP.hang <$> goNormal (InfixOp Highest) f <*> PP.spacedTraverse (goNormal Application) args)
          t -> pure $ l "error: " <> l (show t)
    where
      goNormal prec tm = pretty0 (ac prec Normal im doc) tm
      specialCases term go = do
        prettyDoc2 a term >>= \case
          Just d -> pure d
          Nothing -> notDoc go
        where
          notDoc go = do
            env <- ask
            let -- This predicate controls which binary functions we render as infix
                -- operators. At the moment the policy is just to render symbolic
                -- operators as infix.
                binaryOpsPred :: Term3 v PrintAnnotation -> Bool
                binaryOpsPred = \case
                  Ref' r -> isSymbolic $ PrettyPrintEnv.termName env.ppe (Referent.Ref r)
                  Var' v -> isSymbolic $ HQ.unsafeFromVar v
                  _ -> False
                -- Gets the precedence of an infix operator, if it has one.
                termPrecedence :: Term3 v PrintAnnotation -> Maybe Precedence
                termPrecedence = \case
                  Ref' r ->
                    HQ.toName (PrettyPrintEnv.termName env.ppe (Referent.Ref r))
                      >>= operatorPrecedence
                        . NameSegment.toEscapedText
                        . Name.lastSegment
                  Var' v ->
                    HQ.toName (HQ.unsafeFromVar v)
                      >>= operatorPrecedence
                        . NameSegment.toEscapedText
                        . Name.lastSegment
                  _ -> Nothing
                prettyBinaryApp ctx term =
                  case (term, binaryOpsPred) of
                    BinaryAppPred' f a b ->
                      let prec = termPrecedence f
                          p = precedence ctx
                          im = imports ctx
                          doc = docContext ctx
                       in case unBinaryAppsPred' (term, binaryOpsPred) of
                            -- Only render infix operators as a table
                            -- if there's more than one of the same
                            -- operator in a row.
                            Just (apps@(_ : _ : _), lastArg) -> do
                              prettyLast <- pretty0 (ac (fromMaybe (InfixOp Highest) prec) Normal im doc) lastArg
                              prettyApps <- binaryApps apps prettyLast
                              pure $ paren (p > fromMaybe (InfixOp Lowest) prec) prettyApps
                            _ -> do
                              prettyF <- pretty0 (AmbientContext Application Normal Infix im doc False) f
                              prettyA <- prettyBinaryApp (ac (fromMaybe (InfixOp Lowest) prec) Normal im doc) a
                              -- We increment the precedence for the right-hand side
                              -- since we want parens if the right-hand side is an
                              -- infix operator app with the same precedence as the
                              -- current operator.
                              prettyB <- prettyBinaryApp (ac (maybe (InfixOp Highest) increment prec) Normal im doc) b
                              pure . parenNoGroup (p > fromMaybe (InfixOp Lowest) prec) $
                                (prettyA <> " " <> prettyF <> " " <> prettyB) `PP.orElse` (prettyA <> "\n" <> PP.indent "  " (prettyF <> " " <> prettyB))
                    _ -> pretty0 ctx term
                unBinaryAppsPred' ::
                  ( Term3 v PrintAnnotation,
                    Term3 v PrintAnnotation -> Bool
                  ) ->
                  Maybe
                    ( [ ( Term3 v PrintAnnotation,
                          Term3 v PrintAnnotation
                        )
                      ],
                      Term3 v PrintAnnotation
                    )
                unBinaryAppsPred' (t, isInfix) =
                  go t isInfix
                  where
                    go t pred =
                      case unBinaryAppPred (t, pred) of
                        Just (f, x, y) ->
                          -- We only chain together infix operators in a table
                          -- if they are literally the same operator.
                          let inChain g = isInfix g && (g == f)
                              l = unBinaryAppsPred' (x, inChain)
                           in case l of
                                Just (as, xLast) -> Just ((xLast, f) : as, y)
                                Nothing -> Just ([(x, f)], y)
                        Nothing -> Nothing

                -- Render a binary infix operator sequence, like [(a2, f2), (a1, f1)],
                -- meaning (a1 `f1` a2) `f2` (a3 rendered by the caller), producing
                -- "a1 `f1` a2 `f2`".  Except the operators are all symbolic, so we won't
                -- produce any backticks.  We build the result out from the right,
                -- starting at `f2`.
                binaryApps ::
                  [(Term3 v PrintAnnotation, Term3 v PrintAnnotation)] ->
                  Pretty SyntaxText ->
                  m (Pretty SyntaxText)
                binaryApps xs last =
                  do
                    let xs' = reverse xs
                    psh <- join <$> traverse (uncurry (r (InfixOp Lowest))) (take 1 xs')
                    pst <- join <$> traverse (uncurry (r (InfixOp Highest))) (drop 1 xs')
                    let ps = psh <> pst
                    let unbroken = PP.spaced (ps <> [last])
                        broken = PP.hang (head ps) . PP.column2 . psCols $ tail ps <> [last]
                    pure (unbroken `PP.orElse` broken)
                  where
                    psCols ps = case take 2 ps of
                      [x, y] -> (x, y) : psCols (drop 2 ps)
                      [x] -> [(x, "")]
                      [] -> []
                      _ -> undefined
                    r p a f =
                      sequenceA
                        [ pretty0 (ac (if isBlock a then Top else fromMaybe p (termPrecedence f)) Normal im doc) a,
                          pretty0 (AmbientContext Application Normal Infix im doc False) f
                        ]
            case (term, binaryOpsPred) of
              (DD.Doc, _)
                | doc == MaybeDoc ->
                    if isDocLiteral term
                      then do
                        env <- ask
                        pure (prettyDoc env.ppe im term)
                      else pretty0 (a {docContext = NoDoc}) term
              (TupleTerm' [x], _) -> do
                let conRef = DD.pairCtorRef
                env <- ask
                let name = elideFQN im (PrettyPrintEnv.termName env.ppe conRef)
                let pair = parenIfInfix name ic $ styleHashQualified'' (fmt (S.TermReference conRef)) name
                x' <- pretty0 (ac Application Normal im doc) x
                pure . paren (p >= Application) $
                  pair
                    `PP.hang` PP.spaced [x', fmt (S.TermReference DD.unitCtorRef) "()"]
              (TupleTerm' xs, _) -> do
                let tupleLink p = fmt (S.TypeReference DD.pairRef) p
                let comma = tupleLink ", " `PP.orElse` ("\n" <> tupleLink ", ")
                pelems <- traverse (fmap (PP.indentNAfterNewline 2) . goNormal Annotation) xs
                let clist = PP.sep comma pelems
                let open = tupleLink "(" `PP.orElse` tupleLink "( "
                let close = tupleLink ")" `PP.orElse` ("\n" <> tupleLink ")")
                pure $ PP.group (open <> clist <> close)
              (App' f@(Builtin' "Any.Any") arg, _) ->
                paren (p >= Application) <$> (PP.hang <$> goNormal (InfixOp Highest) f <*> goNormal Application arg)
              (DD.Rewrites' rs, _) -> do
                let kw = fmt S.ControlKeyword "@rewrite"
                    arr = fmt S.ControlKeyword "==>"
                    control = fmt S.ControlKeyword
                    sub kw lhs = PP.sep " " <$> sequence [pure $ control kw, goNormal Annotation lhs, pure arr]
                    go (DD.RewriteTerm' lhs rhs) = PP.hang <$> sub "term" lhs <*> goNormal Annotation rhs
                    go (DD.RewriteCase' lhs rhs) = PP.hang <$> sub "case" lhs <*> goNormal Annotation rhs
                    go (DD.RewriteSignature' vs lhs rhs) = do
                      lhs <- TypePrinter.pretty0 im 0 lhs
                      PP.hang (PP.sep " " (stuff lhs)) <$> TypePrinter.pretty0 im 0 rhs
                      where
                        stuff lhs =
                          [control "signature"]
                            <> [fmt S.Var (PP.text (Var.name v)) | v <- vs]
                            <> (if null vs then [] else [fmt S.TypeOperator "."])
                            <> [lhs, arr]
                    go tm = goNormal Application tm
                PP.hang kw <$> fmap PP.lines (traverse go rs)
              (Bytes' bs, _) ->
                pure $ PP.group $ fmt S.BytesLiteral "0xs" <> PP.shown (Bytes.fromWord8s (map fromIntegral bs))
              binApp@(BinaryAppPred' {}) -> do
                v <- PP.group <$> prettyBinaryApp a (fst binApp)
                pure v
              (And' a b, _) -> do
                let prec = operatorPrecedence "&&"
                    prettyF = fmt S.ControlKeyword "&&"
                prettyA <- pretty0 (ac (fromMaybe (InfixOp Lowest) prec) Normal im doc) a
                prettyB <- pretty0 (ac (fromMaybe (InfixOp Highest) prec) Normal im doc) b
                pure . parenNoGroup (p > fromMaybe (InfixOp Lowest) prec) $
                  (prettyA <> " " <> prettyF <> " " <> prettyB)
                    `PP.orElse` (prettyA <> "\n" <> PP.indent "  " (prettyF <> " " <> prettyB))
              (Or' a b, _) -> do
                let prec = operatorPrecedence "||"
                    prettyF = fmt S.ControlKeyword "||"
                prettyA <- pretty0 (ac (fromMaybe (InfixOp Lowest) prec) Normal im doc) a
                prettyB <- pretty0 (ac (fromMaybe (InfixOp Highest) prec) Normal im doc) b
                pure . parenNoGroup (p > fromMaybe (InfixOp Lowest) prec) $
                  (prettyA <> " " <> prettyF <> " " <> prettyB)
                    `PP.orElse` (prettyA <> "\n" <> PP.indent "  " (prettyF <> " " <> prettyB))
              {-
              When a delayed computation block is passed to a function as the last argument
              in a context where the ambient precedence is low enough, we can elide parentheses
              around it and use a "soft hang" to put the `'let` on the same line as the function call.
              This looks nice.

                forkAt usEast 'let
                  x = thing1
                  y = thing2
                  ...

              instead of the ugly but effective

                forkAt
                  usEast
                  ('let
                    x = thing1
                    y = thing2
                    ...)
              -}
              (App' x (Constructor' (ConstructorReference DD.UnitRef 0)), _) | isLeaf x -> do
                px <- pretty0 (ac (if isBlock x then Annotation else InfixOp Highest) Normal im doc) x
                pure . paren (p >= Prefix || isBlock x && p >= (InfixOp Lowest)) $
                  px <> fmt S.Unit (l "()")
              (Apps' f (unsnoc -> Just (args, lastArg)), _)
                | isSoftHangable lastArg -> do
                    fun <- goNormal (InfixOp Highest) f
                    args' <- traverse (goNormal Application) args
                    lastArg' <- goNormal Annotation lastArg
                    let softTab = PP.softbreak <> ("" `PP.orElse` "  ")
                    pure . paren (p >= (InfixOp Lowest)) $
                      PP.group (PP.group (PP.group (PP.sep softTab (fun : args') <> softTab)) <> lastArg')
              _other -> case (term, nonForcePred) of
                AppsPred' f args ->
                  paren (p >= Application) <$> do
                    f' <- pretty0 (ac Application Normal im doc) f
                    args' <- PP.spacedTraverse (pretty0 (ac Application Normal im doc)) args
                    pure $ f' `PP.hang` args'
                _other -> case (term, \v -> nonUnitArgPred v && not (isDelay term)) of
                  (LamsNamedMatch' [] branches, _) -> do
                    pbs <- printCase im doc branches
                    pure . paren (p >= InfixOp Lowest) $
                      PP.group (fmt S.ControlKeyword "cases") `PP.hang` pbs
                  LamsNamedPred' vs body -> do
                    prettyBody <- pretty0 (ac Control Normal im doc) body
                    let hang = case body of
                          Delay' (Lets' _ _) -> PP.softHang
                          Lets' _ _ -> PP.softHang
                          Match' _ _ -> PP.softHang
                          _ -> PP.hang
                    pure . paren (p >= InfixOp Lowest) $
                      PP.group (varList vs <> fmt S.ControlKeyword " ->") `hang` prettyBody
                  _other -> go term

      isDelay (Delay' _) = True
      isDelay _ = False
      sepList' f sep xs = fold . intersperse sep <$> traverse f xs
      varList = runIdentity . sepList' (Identity . PP.text . Var.name) PP.softbreak

      nonForcePred :: Term3 v PrintAnnotation -> Bool
      nonForcePred = \case
        Constructor' (ConstructorReference DD.DocRef _) -> False
        _ -> True

      nonUnitArgPred :: (Var v) => v -> Bool
      nonUnitArgPred v = Var.name v /= "()"

printLet ::
  (MonadPretty v m) =>
  AmbientContext ->
  BlockContext ->
  [LetBindings v (Term3 v PrintAnnotation)] ->
  Term3 v PrintAnnotation ->
  [Pretty SyntaxText] ->
  m (Pretty SyntaxText)
printLet context sc bs e uses = do
  bs <- traverse (printLetBindings bindingContext) bs
  body <- body e
  pure . paren (sc /= Block && context.precedence >= Top) . letIntro $ PP.lines (uses <> concat bs <> body)
  where
    bindingContext :: AmbientContext
    bindingContext =
      ac Bottom Normal context.imports context.docContext
    body = \case
      Constructor' (ConstructorReference DD.UnitRef 0) | context.elideUnit -> pure []
      e -> List.singleton <$> pretty0 (ac Annotation Normal context.imports context.docContext) e
    letIntro = case sc of
      Block -> id
      Normal -> (fmt S.ControlKeyword "let" `PP.hang`)

printLetBindings ::
  (MonadPretty v m) =>
  AmbientContext ->
  LetBindings v (Term3 v PrintAnnotation) ->
  m [Pretty SyntaxText]
printLetBindings context = \case
  LetBindings bindings -> traverse (printLetBinding context) bindings
  LetrecBindings bindings ->
    let boundVars = map fst bindings
     in traverse (printLetrecBinding context boundVars) bindings

printLetBinding :: (MonadPretty v m) => AmbientContext -> (v, Term3 v PrintAnnotation) -> m (Pretty SyntaxText)
printLetBinding context (v, binding)
  | Var.isAction v = pretty0 context binding
  | otherwise =
      renderPrettyBinding <$> withBoundTerm v (prettyBinding0' context (HQ.unsafeFromVar v1) binding)
  where
    v1 = Var.reset v

printLetrecBinding :: (MonadPretty v m) => AmbientContext -> [v] -> (v, Term3 v PrintAnnotation) -> m (Pretty SyntaxText)
printLetrecBinding context vs (v, binding) =
  renderPrettyBinding <$> withBoundTerms vs (prettyBinding0' context (HQ.unsafeFromVar (Var.reset v)) binding)

prettyPattern ::
  forall v loc.
  (Var v) =>
  PrettyPrintEnv ->
  AmbientContext ->
  Precedence ->
  [v] ->
  Pattern loc ->
  (Pretty SyntaxText, [v])
-- vs is the list of pattern variables used by the pattern, plus possibly a
-- tail of variables it doesn't use.  This tail is the second component of
-- the return value.
prettyPattern n c@AmbientContext {imports = im} p vs patt = case patt of
  Pattern.Char _ c ->
    ( fmt S.CharLiteral $
        l $ case showEscapeChar c of
          Just c -> "?\\" ++ [c]
          Nothing -> '?' : [c],
      vs
    )
  Pattern.Unbound _ -> (fmt S.DelimiterChar $ l "_", vs)
  Pattern.Var _ ->
    case vs of
      (v : tail_vs) -> (fmt S.Var $ l $ Var.nameStr (Var.reset v), tail_vs)
      _ -> error "prettyPattern: Expected at least one var"
  Pattern.Boolean _ b -> (fmt S.BooleanLiteral $ if b then l "true" else l "false", vs)
  Pattern.Int _ i -> (fmt S.NumericLiteral $ (if i >= 0 then l "+" else mempty) <> l (show i), vs)
  Pattern.Nat _ u -> (fmt S.NumericLiteral $ l $ show u, vs)
  Pattern.Float _ f -> (fmt S.NumericLiteral $ l $ show f, vs)
  Pattern.Text _ t -> (fmt S.TextLiteral $ l $ show t, vs)
  TuplePattern pats
    | length pats /= 1 ->
        let (pats_printed, tail_vs) = patterns Bottom vs pats
         in (PP.parenthesizeCommas pats_printed, tail_vs)
  Pattern.Constructor _ ref [] ->
    (styleHashQualified'' (fmt $ S.TermReference conRef) name, vs)
    where
      name = elideFQN im $ PrettyPrintEnv.termName n conRef
      conRef = Referent.Con ref CT.Data
  Pattern.Constructor _ ref pats ->
    let (pats_printed, tail_vs) = patternsSep Application PP.softbreak vs pats
        name = elideFQN im $ PrettyPrintEnv.termName n conRef
        conRef = Referent.Con ref CT.Data
     in ( paren (p >= Application) $
            styleHashQualified'' (fmt $ S.TermReference conRef) name
              `PP.hang` pats_printed,
          tail_vs
        )
  Pattern.As _ pat ->
    case vs of
      (v : tail_vs) ->
        let (printed, eventual_tail) = prettyPattern n c Prefix tail_vs pat
         in (paren (p >= Prefix) (fmt S.Var (l $ Var.nameStr (Var.reset v)) <> fmt S.DelimiterChar (l "@") <> printed), eventual_tail)
      _ -> error "prettyPattern: Expected at least one var"
  Pattern.EffectPure _ pat ->
    let (printed, eventual_tail) = prettyPattern n c Bottom vs pat
     in (PP.sep " " [fmt S.DelimiterChar "{", printed, fmt S.DelimiterChar "}"], eventual_tail)
  Pattern.EffectBind _ ref pats k_pat ->
    let (pats_printed, tail_vs) = patternsSep Application PP.softbreak vs pats
        (k_pat_printed, eventual_tail) = prettyPattern n c Annotation tail_vs k_pat
        name = elideFQN im $ PrettyPrintEnv.termName n conRef
        conRef = Referent.Con ref CT.Effect
     in ( PP.group
            ( PP.sep " " . PP.nonEmpty $
                [ fmt S.DelimiterChar "{",
                  styleHashQualified'' (fmt (S.TermReference conRef)) name,
                  pats_printed,
                  fmt S.ControlKeyword "->",
                  k_pat_printed,
                  fmt S.DelimiterChar "}"
                ]
            ),
          eventual_tail
        )
  Pattern.SequenceLiteral _ pats ->
    let (pats_printed, tail_vs) = patternsSep Bottom (fmt S.DelimiterChar ", ") vs pats
     in (fmt S.DelimiterChar "[" <> pats_printed <> fmt S.DelimiterChar "]", tail_vs)
  Pattern.SequenceOp _ l op r ->
    let (pl, lvs) = prettyPattern n c p vs l
        (pr, rvs) = prettyPattern n c (increment p) lvs r
        f i s = (paren (p >= i) (pl <> " " <> fmt (S.Op op) s <> " " <> pr), rvs)
     in case op of
          Pattern.Cons -> f Annotation "+:"
          Pattern.Snoc -> f Annotation ":+"
          Pattern.Concat -> f Annotation "++"
  where
    l :: (IsString s) => String -> s
    l = fromString
    patterns p vs (pat : pats) =
      let (printed, tail_vs) =
            prettyPattern n c p vs pat
          (rest_printed, eventual_tail) = patterns p tail_vs pats
       in (printed : rest_printed, eventual_tail)
    patterns _ vs [] = ([], vs)
    patternsSep p sep vs pats = case patterns p vs pats of
      (printed, tail_vs) -> (PP.sep sep printed, tail_vs)

type MatchCase' ann tm = ([Pattern ann], Maybe tm, tm)

arity1Branches :: [MatchCase ann tm] -> [MatchCase' ann tm]
arity1Branches bs = [([pat], guard, body) | MatchCase pat guard body <- bs]

-- Groups adjacent cases with the same pattern together,
-- for easier pretty-printing, for instance:
--
--   Foo x y | blah1 x -> body1
--   Foo x y | blah2 y -> body2
--
-- becomes
--
--   Foo x y, [x,y], [(blah1 x, body1), (blah2 y, body2)]
groupCases ::
  (Ord v) =>
  [MatchCase' () (Term3 v ann)] ->
  [([Pattern ()], [v], [(Maybe (Term3 v ann), ([v], Term3 v ann))])]
groupCases = \cases
  [] -> []
  ms@((p1, _, AbsN' vs1 _) : _) -> go (p1, vs1) [] ms
  where
    go (p0, vs0) acc [] = [(p0, vs0, reverse acc)]
    go (p0, vs0) acc ms@((p1, g1, AbsN' vs body) : tl)
      | p0 == p1 && vs == vs0 = go (p0, vs0) ((g1, (vs, body)) : acc) tl
      | otherwise = (p0, vs0, reverse acc) : groupCases ms

printCase ::
  forall m v.
  (MonadPretty v m) =>
  Imports ->
  DocLiteralContext ->
  [MatchCase' () (Term3 v PrintAnnotation)] ->
  m (Pretty SyntaxText)
printCase im doc ms =
  PP.orElse
    <$> (PP.lines . alignGrid True <$> grid)
    <*> (PP.lines . alignGrid False <$> grid)
  where
    justify rows =
      zip (fmap fst . PP.align' $ fmap alignPatterns rows) $ fmap gbs rows
      where
        alignPatterns (p, _, _) = (p, Just "")
        gbs (_, gs, bs) = zip gs bs
    nojustify = map f
      where
        f (p, gs, bs) = (p, zip gs bs)
    alignGrid alignArrows grid =
      fmap alignCase $ if alignArrows then justify grid else nojustify grid
      where
        alignCase (p, gbs) =
          if not (null (drop 1 gbs))
            then PP.hang p guardBlock
            else p <> guardBlock
          where
            guardBlock =
              PP.lines $
                fmap
                  ( \(g, (a, b)) ->
                      PP.hang
                        ( PP.group
                            (g <> (if alignArrows then "" else " ") <> a)
                        )
                        b
                  )
                  justified
            justified = PP.leftJustify $ fmap (\(g, b) -> (g, (arrow, b))) gbs
    grid = traverse go (groupCases ms)
    patLhs :: PrettyPrintEnv -> [v] -> [Pattern ()] -> Pretty SyntaxText
    patLhs ppe vs = \cases
      [pat] -> PP.group (fst (prettyPattern ppe (ac Annotation Block im doc) Bottom vs pat))
      pats -> PP.group
        . PP.sep (PP.indentAfterNewline "  " $ "," <> PP.softbreak)
        . (`evalState` vs)
        . for pats
        $ \pat -> do
          vs <- State.get
          let (p, rem) = prettyPattern ppe (ac Annotation Block im doc) Bottom vs pat
          State.put rem
          pure p
    arrow = fmt S.ControlKeyword "->"
    -- If there's multiple guarded cases for this pattern, prints as:
    -- MyPattern x y
    --   | guard 1        -> 1
    --   | otherguard x y -> 2
    --   | otherwise      -> 3
    go (pats, vs, unzip -> (guards, bodies)) = do
      guards' <- traverse printGuard guards
      bodies' <- traverse printBody bodies
      env <- ask
      pure (patLhs env.ppe vs pats, guards', bodies')
      where
        noGuards = all (== Nothing) guards
        printGuard Nothing | noGuards = pure mempty
        printGuard Nothing =
          pure $ fmt S.DelimiterChar "|" <> " " <> fmt S.ControlKeyword "otherwise"
        printGuard (Just (ABT.AbsN' _ g)) =
          -- strip off any Abs-chain around the guard, guard variables are rendered
          -- like any other variable, ex: case Foo x y | x < y -> ...
          PP.spaceIfNeeded (fmt S.DelimiterChar "|")
            <$> pretty0 (ac Control Normal im doc) g
        printBody (vs, body) = withBoundTerms vs (pretty0 (ac Annotation Block im doc) body)

-- A pretty term binding, split into the type signature (possibly empty) and the term.
data PrettyBinding = PrettyBinding
  { typeSignature :: Maybe (Pretty SyntaxText),
    term :: Pretty SyntaxText
  }

-- Render a pretty binding.
renderPrettyBinding :: PrettyBinding -> Pretty SyntaxText
renderPrettyBinding PrettyBinding {typeSignature, term} =
  case typeSignature of
    Nothing -> term
    Just ty -> PP.lines [ty, term]

-- Render a pretty binding without a type signature.
renderPrettyBindingWithoutTypeSignature :: PrettyBinding -> Pretty SyntaxText
renderPrettyBindingWithoutTypeSignature PrettyBinding {term} =
  term

-- | Render a binding, producing output of the form
--
-- foo : t -> u
-- foo a = ...
--
-- The first line is only output if the term has a type annotation as the
-- outermost constructor.
--
-- Binary functions with symbolic names are output infix, as follows:
--
-- (+) : t -> t -> t
-- a + b = ...
prettyBinding ::
  (Var v) =>
  PrettyPrintEnv ->
  HQ.HashQualified Name ->
  Term2 v at ap v a ->
  Pretty SyntaxText
prettyBinding =
  prettyBinding_ renderPrettyBinding

-- | Like 'prettyBinding', but elides the type signature (if any).
prettyBindingWithoutTypeSignature ::
  (Var v) =>
  PrettyPrintEnv ->
  HQ.HashQualified Name ->
  Term2 v at ap v a ->
  Pretty SyntaxText
prettyBindingWithoutTypeSignature =
  prettyBinding_ renderPrettyBindingWithoutTypeSignature

prettyBinding_ ::
  (Var v) =>
  (PrettyBinding -> Pretty SyntaxText) ->
  PrettyPrintEnv ->
  HQ.HashQualified Name ->
  Term2 v at ap v a ->
  Pretty SyntaxText
prettyBinding_ go ppe n tm =
  runPretty (avoidShadowing tm ppe) . fmap go $ prettyBinding0 (ac Basement Block Map.empty MaybeDoc) n tm

prettyBinding' ::
  (Var v) =>
  PrettyPrintEnv ->
  Width ->
  HQ.HashQualified Name ->
  Term v a ->
  ColorText
prettyBinding' ppe width v t =
  PP.render width . PP.syntaxToColor $ prettyBinding ppe v t

prettyBinding0 ::
  (HasCallStack, MonadPretty v m) =>
  AmbientContext ->
  HQ.HashQualified Name ->
  Term2 v at ap v a ->
  m PrettyBinding
prettyBinding0 ac v tm = do
  env <- ask
  local (set #freeTerms (ABT.freeVars tm)) (prettyBinding0' ac v (printAnnotate env.ppe tm))

prettyBinding0' ::
  (MonadPretty v m) =>
  AmbientContext ->
  HQ.HashQualified Name ->
  Term3 v PrintAnnotation ->
  m PrettyBinding
prettyBinding0' a@AmbientContext {imports = im, docContext = doc} v term =
  go (symbolic && isBinary term) term
  where
    go infix' binding =
      case binding of
        Ann' tm tp -> do
          -- If the term is an annotated function,
          -- we want to print the type signature on the previous line.
          -- The TypePrinter.pretty0 function prints the type, and uses a
          -- Reader monad with (Set v) in it to track which type variables are
          -- bound in the outer scope. We use that to determine if the type
          -- printer should avoid capture of those variables.
          let avoidCapture = case tp of
                ForallsNamed' vs _ -> addTypeVars vs
                _ -> id
          tp' <- TypePrinter.pretty0 im (-1) tp
          tm' <- avoidCapture (prettyBinding0' a v tm)
          pure
            PrettyBinding
              { typeSignature = Just (PP.group (renderName v <> PP.hang (fmt S.TypeAscriptionColon " :") tp')),
                term = PP.group (renderPrettyBinding tm')
              }
        LamsNamedMatch' vs branches -> do
          branches' <- printCase im doc branches
          pure
            PrettyBinding
              { typeSignature = Nothing,
                term =
                  PP.group $
                    PP.group
                      ( defnLhs v vs
                          <> fmt S.BindingEquals " ="
                          <> " "
                          <> fmt
                            S.ControlKeyword
                            "cases"
                      )
                      `PP.hang` branches'
              }
        LamsNamedOrDelay' vs body -> do
          prettyBody <- pretty0 (ac (precedence a) Block im doc) body
          case body of
            -- allow soft hangs when first line is a function application
            -- that ends in a delay or other soft-hangable element
            --   foo = fork loc do
            --     ...
            --   foo =
            --     fork loc do
            --       ...
            Apps' _f args
              | Just (_, last) <- unsnoc args,
                isSoftHangable last ->
                  pure $
                    PrettyBinding
                      { typeSignature = Nothing,
                        term =
                          PP.group $
                            PP.group (defnLhs v vs <> fmt S.BindingEquals " = ")
                              <> prettyBody
                                `PP.orElse` ("\n" <> PP.indentN 2 prettyBody)
                      }
            _ ->
              pure $
                -- Special case for 'let being on the same line
                let hang = if isSoftHangable body then PP.softHang else PP.hang
                 in PrettyBinding
                      { typeSignature = Nothing,
                        term =
                          PP.group $
                            PP.group (defnLhs v vs <> fmt S.BindingEquals " =") `hang` prettyBody
                      }
        t -> error ("prettyBinding0: unexpected term: " ++ show t)
      where
        defnLhs v vs
          | infix' = case vs of
              x : y : _ ->
                PP.sep
                  " "
                  [ fmt S.Var $ PP.text (Var.name x),
                    styleHashQualified'' (fmt $ S.HashQualifier v) $ elideFQN im v,
                    fmt S.Var $ PP.text (Var.name y)
                  ]
              [x] ->
                PP.sep
                  " "
                  [ renderName v,
                    fmt S.Var $ PP.text (Var.name x)
                  ]
              _ -> l "error"
          | null vs = renderName v
          | otherwise = renderName v `PP.hang` args vs
        args = PP.spacedMap $ fmt S.Var . PP.text . Var.name
        renderName n =
          let n' = elideFQN im n
           in parenIfInfix n' NonInfix $ styleHashQualified'' (fmt $ S.HashQualifier n') n'
    symbolic = isSymbolic v
    isBinary = \case
      Ann' tm _ -> isBinary tm
      LamsNamedMatch' vs _ -> length vs == 1
      LamsNamedOrDelay' vs _ -> length vs == 2
      _ -> False -- unhittable

isDocLiteral :: Term3 v PrintAnnotation -> Bool
isDocLiteral term = case term of
  DD.DocJoin segs -> all isDocLiteral segs
  DD.DocBlob _ -> True
  DD.DocLink (DD.LinkTerm (TermLink' _)) -> True
  DD.DocLink (DD.LinkType (TypeLink' _)) -> True
  DD.DocSource (DD.LinkTerm (TermLink' _)) -> True
  DD.DocSource (DD.LinkType (TypeLink' _)) -> True
  DD.DocSignature (TermLink' _) -> True
  DD.DocEvaluate (TermLink' _) -> True
  Ref' _ -> True -- @[include]
  _ -> False

-- Similar to DisplayValues.displayDoc, but does not follow and expand references.
prettyDoc :: (Var v) => PrettyPrintEnv -> Imports -> Term3 v a -> Pretty SyntaxText
prettyDoc n im term =
  mconcat
    [ fmt S.DocDelimiter $ l "[: ",
      go term,
      spaceUnlessBroken,
      fmt S.DocDelimiter $ l ":]"
    ]
  where
    go (DD.DocJoin segs) = foldMap go segs
    go (DD.DocBlob txt) = PP.paragraphyText (escaped txt)
    go (DD.DocLink (DD.LinkTerm (TermLink' r))) =
      fmt S.DocDelimiter (l "@") <> (fmt $ S.TermReference r) (fmtTerm r)
    go (DD.DocLink (DD.LinkType (TypeLink' r))) =
      fmt S.DocDelimiter (l "@") <> (fmt $ S.TypeReference r) (fmtType r)
    go (DD.DocSource (DD.LinkTerm (TermLink' r))) =
      atKeyword "source" <> fmtTerm r
    go (DD.DocSource (DD.LinkType (TypeLink' r))) =
      atKeyword "source" <> fmtType r
    go (DD.DocSignature (TermLink' r)) =
      atKeyword "signature" <> fmtTerm r
    go (DD.DocEvaluate (TermLink' r)) =
      atKeyword "evaluate" <> fmtTerm r
    go (Ref' r) = atKeyword "include" <> fmtTerm (Referent.Ref r)
    go _ = l $ "(invalid doc literal: " ++ show term ++ ")"
    fmtName s = styleHashQualified'' (fmt $ S.HashQualifier s) $ elideFQN im s
    fmtTerm r = fmtName $ PrettyPrintEnv.termName n r
    fmtType r = fmtName $ PrettyPrintEnv.typeName n r
    atKeyword w =
      fmt S.DocDelimiter (l "@[")
        <> fmt S.DocKeyword (l w)
        <> fmt S.DocDelimiter (l "] ")
    escaped = Text.replace "@" "\\@" . Text.replace ":]" "\\:]"
    spaceUnlessBroken = PP.orElse " " ""

paren :: Bool -> Pretty SyntaxText -> Pretty SyntaxText
paren b s = PP.group $ parenNoGroup b s

parenNoGroup :: Bool -> Pretty SyntaxText -> Pretty SyntaxText
parenNoGroup True s = fmt S.Parenthesis "(" <> s <> fmt S.Parenthesis ")"
parenNoGroup False s = s

parenIfInfix ::
  HQ.HashQualified Name ->
  InfixContext ->
  (Pretty SyntaxText -> Pretty SyntaxText)
parenIfInfix name ic =
  if isSymbolic name && ic == NonInfix then paren True else id

l :: (IsString s) => String -> Pretty s
l = fromString

isSymbolic :: HQ.HashQualified Name -> Bool
isSymbolic =
  maybe False Name.isSymboly . HQ.toName

emptyAc :: AmbientContext
emptyAc = ac Bottom Normal Map.empty MaybeDoc

emptyBlockAc :: AmbientContext
emptyBlockAc = ac Bottom Block Map.empty MaybeDoc

ac :: Precedence -> BlockContext -> Imports -> DocLiteralContext -> AmbientContext
ac prec bc im doc = AmbientContext prec bc NonInfix im doc False

fmt :: S.Element r -> Pretty (S.SyntaxText' r) -> Pretty (S.SyntaxText' r)
fmt = PP.withSyntax

{-
   # FQN elision

   The term pretty-printer inserts `use` statements in some circumstances, to
   avoid the need for using fully-qualified names (FQNs) everywhere.  The
   following is an explanation and specification, as developed in issue #285.

   As an example, instead of

     foo p q r =
       if p then Util.bar q else Util.bar r

   we actually output the following.

     foo p q r =
       use Util bar
       if p then bar q else bar r

   Here, the `use` statement `use Util bar` has been inserted at the start of
   the block statement containing the `if`.  Within that scope, `Util.bar` can
   be referred to just with `bar`.  We say `Util` is the prefix, and `bar` is
   the suffix.

   When choosing where to place `use` statements, the pretty-printer tries to
   - float them down, deeper into the syntax tree, to keep them visually close
     to the use sites ('usages') of the names involved, but also tries to
   - minimize the number of repetitions of `use` statements for the same names
     by floating them up, towards the top of the syntax tree, so that one
     `use` statement takes effect over more name usages.

   It avoids producing output like the following.

     foo p q r =
       use My bar
       if p then bar q else Your.bar r

   Here `My.bar` is imported with a `use` statement, but `Your.bar` is not.
   We avoid this because it would be easy to misread `bar` as meaning
   `Your.bar`.  Instead both names are output fully qualified.

   This means that a `use` statement is only emitted for a name
   when the suffix is unique, across all the names referenced in the scope of
   the `use` statement.

   We don't emit a `use` statement for a name if it only occurs once within
   the scope (unless it's an infix operator, since they look nicer without
   a namespace qualifier.)

   The emitted code does not depend on Type-Driven Name Resolution (TDNR).
   For example, we emit
     foo =
       use Nat +
       1 + 2
   even though TDNR means that `foo = 1 + 2` would have had the same
   meaning.  That avoids the reader having to run typechecker logic in their
   head in order to know what functions are being called.

   Multi-level name qualification is allowed - like `Foo.Bar.baz`.  The
   pretty-printer tries to strip off as many sections of the prefix as
   possible, without causing a clash with other names.  If more sections
   can be stripped off, further down the tree, then it does this too.

   ## Specification

   We output a `use` statement for prefix P and suffix S at a given scope if
     - the scope is a block statement (so the `use` is syntactically valid)
     - the number of usages of the thing referred to by P.S within the scope
       - is > 1, or
       - is 1, and S is an infix operator
     - [uniqueness] there is no other Q with Q.S used in that scope
     - there is no longer prefix PP (and suffix s, with PP.s == P.S) which
       satisfies uniqueness
     - [narrowness] there is no block statement further down inside this one
       which contains all of the usages.

   Use statements in a block statement are sorted alphabetically by prefix.
   Suffixes covered by a single use statement are sorted alphabetically.
   Note that each `use` line cannot be line-broken.  Ideally they would
   fit the available space by splitting into multiple separate `use` lines.

   ## Algorithm

   Bubbling up from the leaves of the syntax tree, we calculate for each
   node, a `Map Suffix (Map Prefix Int)` (the 'usages map'), where the `Int`
   is the number of usages of Prefix.Suffix at/under that node.  (Note that
   a usage of `A.B.c` corresponds to two entries in the outer map.)  See
   `printAnnotate`.

   Once we have this decoration on all the terms, we start pretty-printing.
   As we recurse back down through the tree, we keep a `Map Name Suffix` (the
   'imports map'), to record the effect of all the `use` statements we've added
   in the nodes above.  When outputting names, we check this map to work out
   how to render them, using any suffix we find, or else falling back to the
   FQN.  At each block statement, each suffix in that term's usages map is a
   candidate to be imported with a use statement, subject to the various
   rules in the specification.

   # Debugging

   Start by enabling the tracing in elideFQN in PrettyPrintEnv.hs.

   There's also tracing in allInSubBlock to help when the narrowness check
   is playing up.

   # Semantics of imports

   Here is some background on how imports work.

   `use XYZ blah` brings `XYZ.blah` into scope, bound to the name `blah`. More
   generally, `use` is followed by a FQN prefix, then the local suffix.
   Concatenate the FQN prefix with the local suffix, with a dot between them,
   and you get the FQN, which is bound to the name equal to the local suffix.

   `use XYZ blah qux` is equivalent to the two statements (and this
   generalizes for any N symbols):
     use XYZ blah
     use XYZ qux

   This syntax works the same even if XYZ or blah have dots in them, so:
   `use Util.External My.Foo` brings `Util.External.My.Foo` into scope, bound
   to the name `My.Foo`.

   That's it. No wildcard imports, imports that do renaming, etc. We can
   consider adding some features like this later.
-}

newtype PrintAnnotation = PrintAnnotation
  { -- For each suffix that appears in/under this term, the set of prefixes
    -- used with that suffix, and how many times each occurs.
    usages :: Map Suffix (Map Prefix Int)
  }
  deriving (Show)

instance Semigroup PrintAnnotation where
  PrintAnnotation {usages = a} <> PrintAnnotation {usages = b} =
    PrintAnnotation {usages = Map.unionWith f a b}
    where
      f a' b' = Map.unionWith (+) a' b'

instance Monoid PrintAnnotation where
  mempty = PrintAnnotation {usages = Map.empty}

suffixCounterTerm :: (HasCallStack, Var v) => PrettyPrintEnv -> Set Name -> Set Name -> Term2 v at ap v a -> PrintAnnotation
suffixCounterTerm n usedTm usedTy = \case
  Ref' r -> countHQ usedTm $ PrettyPrintEnv.termName n (Referent.Ref r)
  Constructor' r | noImportRefs (r ^. ConstructorReference.reference_) -> mempty
  Constructor' r -> countHQ usedTm $ PrettyPrintEnv.termName n (Referent.Con r CT.Data)
  Request' r -> countHQ usedTm $ PrettyPrintEnv.termName n (Referent.Con r CT.Effect)
  Ann' _ t -> countTypeUsages n usedTy t
  Match' _ bs ->
    let pat (MatchCase p _ _) = p
     in foldMap (countPatternUsages n usedTm . pat) bs
  _ -> mempty

suffixCounterType :: (HasCallStack, Var v) => PrettyPrintEnv -> Set Name -> Type v a -> PrintAnnotation
suffixCounterType n used = \case
  Type.Var' v -> countHQ used $ HQ.unsafeFromVar v
  Type.Ref' r | noImportRefs r || r == Type.listRef -> mempty
  Type.Ref' r -> countHQ used $ PrettyPrintEnv.typeName n r
  _ -> mempty

printAnnotate :: (HasCallStack, Var v, Ord v) => PrettyPrintEnv -> Term2 v at ap v a -> Term3 v PrintAnnotation
printAnnotate n tm =
  fmap snd (go (reannotateUp (suffixCounterTerm n usedTermNames usedTypeNames) tm))
  where
    -- See `countHQ` to see how these are used to make sure that
    -- a `use` clause doesn't introduce shadowing of a local variable
    usedTermNames =
      Set.fromList [n | v <- ABT.allVars tm, n <- varToName v]
    usedTypeNames =
      Set.fromList [n | Ann' _ ty <- ABT.subterms tm, v <- ABT.allVars ty, n <- varToName v]
    varToName = toList . Name.parseText . Var.name . Var.reset
    go :: (Ord v) => Term2 v at ap v b -> Term2 v () () v b
    go = extraMap' id (const ()) (const ())

countTypeUsages :: (Var v, Ord v) => PrettyPrintEnv -> Set Name -> Type v a -> PrintAnnotation
countTypeUsages n usedTy t = snd $ annotation $ reannotateUp (suffixCounterType n usedTy) t

countPatternUsages :: (HasCallStack) => PrettyPrintEnv -> Set Name -> Pattern loc -> PrintAnnotation
countPatternUsages n usedTm = Pattern.foldMap' f
  where
    f = \case
      Pattern.Unbound _ -> mempty
      Pattern.Var _ -> mempty
      Pattern.Boolean _ _ -> mempty
      Pattern.Int _ _ -> mempty
      Pattern.Nat _ _ -> mempty
      Pattern.Float _ _ -> mempty
      Pattern.Text _ _ -> mempty
      Pattern.Char _ _ -> mempty
      Pattern.As _ _ -> mempty
      Pattern.SequenceLiteral _ _ -> mempty
      Pattern.SequenceOp {} -> mempty
      Pattern.EffectPure _ _ -> mempty
      Pattern.EffectBind _ r _ _ -> countHQ usedTm $ PrettyPrintEnv.patternName n r
      Pattern.Constructor _ r _ ->
        if noImportRefs (r ^. ConstructorReference.reference_)
          then mempty
          else countHQ usedTm $ PrettyPrintEnv.patternName n r

countHQ :: (HasCallStack) => Set Name -> HQ.HashQualified Name -> PrintAnnotation
countHQ used (HQ.NameOnly n)
  -- Names that are marked 'used' aren't considered for `use` clause insertion
  -- So if a variable 'foo' is used, then we won't insert a `use` clause for
  -- the reference `Qux.quaffle.foo`.
  | Just n' <- Set.lookupLE n used, Name.endsWith n n' = mempty
countHQ _ hq =
  HQ.toName hq & foldMap \n ->
    if Name.isRelative n
      then
        PrintAnnotation
          { usages =
              Map.fromList do
                (p, s) <- Name.splits n
                pure (Name.toText s, Map.singleton (map NameSegment.toEscapedText p) 1)
          }
      else mempty

joinName :: Prefix -> Suffix -> Name
joinName p s = Name.unsafeParseText $ dotConcat $ p ++ [s]

dotConcat :: [Text] -> Text
dotConcat = Text.concat . intersperse "."

-- This predicate is used to keep certain refs out of the FQN elision annotations,
-- so that we don't get `use` statements for them.
--
-- Don't do `use () ()` or `use Pair Pair`.  Tuple syntax generates ().() and Pair.Pair
-- under the covers anyway.  This does mean that if someone is using Pair.Pair directly,
-- then they'll miss out on FQN elision for that.
--
-- Don't do `use builtin.Doc Blob`, `use builtin.Link Term`, or similar.  That avoids
-- unnecessary use statements above Doc literals and termLink/typeLink.
noImportRefs :: Reference -> Bool
noImportRefs r =
  r
    `elem` [ DD.pairRef,
             DD.unitRef,
             DD.docRef,
             DD.linkRef
           ]

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

-- This function gets used each time we start printing a new block statement.
-- It decides what extra imports to introduce (returning the full new set), and
-- determines some pretty-printed lines that looks like
--    use A x
--    use B y
-- providing a `[Pretty SyntaxText] -> Pretty SyntaxText` that prepends those
-- lines to the list of lines provided, and then concatenates them.
calcImports ::
  (Var v, Ord v) =>
  Imports ->
  Term3 v PrintAnnotation ->
  (Imports, [Pretty SyntaxText])
calcImports im tm = (im', render $ getUses result)
  where
    -- The guts of this function is a pipeline of transformations and filters, starting from the
    -- PrintAnnotation we built up in printAnnotate.
    -- In `result`, the Name matches Prefix ++ Suffix; and the Int is the number of usages in this scope.
    -- `result` lists all the names we're going to import, and what Prefix we'll use for each.
    result :: Map Name (Prefix, Suffix, Int)
    result =
      usages'
        |> uniqueness
        |> enoughUsages
        |> groupAndCountLength
        |> longestPrefix
        |> avoidRepeatsAndClashes
        |> narrowestPossible
    usages' :: Map Suffix (Map Prefix Int)
    usages' = usages $ annotation tm
    -- Keep only names P.S where there is no other Q with Q.S also used in this scope.
    uniqueness :: Map Suffix (Map Prefix Int) -> Map Suffix (Prefix, Int)
    uniqueness m =
      m
        |> Map.filter (\ps -> Map.size ps == 1)
        |> Map.map (head . Map.toList)
    -- Keep only names where the number of usages in this scope
    --   - is > 1, or
    --   - is 1, and S is an infix operator.
    -- Also drop names with an empty prefix.
    lookupOrDie s m = fromMaybe msg (Map.lookup s m)
      where
        msg = error $ "TermPrinter.enoughUsages " <> show (s, m)

    enoughUsages :: Map Suffix (Prefix, Int) -> Map Suffix (Prefix, Int)
    enoughUsages m =
      Map.keys m
        |> filter
          ( \s ->
              let (p, i) = lookupOrDie s m
               in (i > 1 || either (const False) Name.isSymboly (Name.parseTextEither s)) && not (null p)
          )
        |> map (\s -> (s, lookupOrDie s m))
        |> Map.fromList
    -- Group by `Prefix ++ Suffix`, and then by `length Prefix`
    groupAndCountLength :: Map Suffix (Prefix, Int) -> Map (Name, Int) (Prefix, Suffix, Int)
    groupAndCountLength m =
      Map.toList m
        |> map
          ( \(s, (p, i)) ->
              let n = joinName p s
                  l = length p
               in ((n, l), (p, s, i))
          )
        |> Map.fromList
    -- For each k1, choose the v with the largest k2.
    longestPrefix :: (Show k1, Show k2, Ord k1, Ord k2) => Map (k1, k2) v -> Map k1 v
    longestPrefix m =
      let k1s = Set.map fst $ Map.keysSet m
          k2s =
            k1s
              |> Map.fromSet
                ( \k1' ->
                    Map.keysSet m
                      |> Set.filter (\(k1, _) -> k1 == k1')
                      |> Set.map snd
                )
          maxk2s = Map.map maximum k2s
          err k1 k2 =
            error $
              "TermPrinter.longestPrefix not found "
                <> show (k1, k2)
                <> " in "
                <> show maxk2s
       in Map.mapWithKey (\k1 k2 -> fromMaybe (err k1 k2) $ Map.lookup (k1, k2) m) maxk2s
    -- Don't do another `use` for a name for which we've already done one, unless the
    -- new suffix is shorter.
    avoidRepeatsAndClashes :: Map Name (Prefix, Suffix, Int) -> Map Name (Prefix, Suffix, Int)
    avoidRepeatsAndClashes = Map.filterWithKey $
      \n (_, s', _) -> case Map.lookup n im of
        Just s -> Text.length s' < Text.length s
        Nothing -> True
    -- Is there a strictly smaller block term underneath this one, containing all the usages
    -- of some of the names?  Skip emitting `use` statements for those, so we can do it
    -- further down, closer to the use sites.
    narrowestPossible :: Map Name (Prefix, Suffix, Int) -> Map Name (Prefix, Suffix, Int)
    narrowestPossible m = m |> Map.filter (\(p, s, i) -> not $ allInSubBlock tm p s i)
    -- `union` is left-biased, so this can replace existing imports.
    im' = getImportMapAdditions result `Map.union` im
    getImportMapAdditions :: Map Name (Prefix, Suffix, Int) -> Map Name Suffix
    getImportMapAdditions = Map.map (\(_, s, _) -> s)
    getUses :: Map Name (Prefix, Suffix, Int) -> Map Prefix (Set Suffix)
    getUses m =
      Map.elems m
        |> map (\(p, s, _) -> (p, Set.singleton s))
        |> Map.fromListWith Set.union
    render m =
      Map.mapWithKey
        ( \p ss ->
            fmt S.UseKeyword (l "use ")
              <> fmt S.UsePrefix (intercalateMap (l ".") (l . unpack) p)
              <> l " "
              <> fmt S.UseSuffix (intercalateMap (l " ") (l . unpack) (Set.toList ss))
        )
        m
        |> Map.toList
        |> map snd

-- Given a block term and a name (Prefix, Suffix) of interest, is there a
-- strictly smaller blockterm within it, containing all usages of that name?
-- A blockterm is a place where the syntax lets us put a use statement, like the
-- branches of an if/then/else.
-- We traverse the block terms by traversing the whole subtree with ABT.find,
-- and paying attention to those subterms that look like a blockterm.
-- This is complicated by the fact that you can't always tell if a term is a
-- blockterm just by looking at it: in some cases you can only tell when you can
-- see it in the context of the wider term that contains it. So actually we
-- traverse the tree, at each term looking for child terms that are block terms,
-- and see if any of those contain all the usages of the name.
-- Cut out the occurrences of "const id $" to get tracing.
allInSubBlock ::
  (Var v, Ord v) =>
  Term3 v PrintAnnotation ->
  Prefix ->
  Suffix ->
  Int ->
  Bool
allInSubBlock tm p s i =
  let found = concat $ ABT.find finder tm
      result = any (/= tm) found
      tr =
        id
   in tr result
  where
    getUsages t =
      annotation t
        |> usages
        |> Map.lookup s
        |> fmap (Map.lookup p)
        |> join
        |> fromMaybe 0
    finder t =
      let result =
            let i' = getUsages t
             in if i' < i
                  then ABT.Prune
                  else
                    let found = filter hit $ immediateChildBlockTerms t
                     in if i' == i && not (null found)
                          then ABT.Found found
                          else ABT.Continue
          -- children =
          --   concatMap
          --     (\t -> "child: " ++ show t ++ "\n")
          --     ( immediateChildBlockTerms t
          --     )
          tr =
            id
       in tr result
    hit t = getUsages t == i

-- Return any blockterms at or immediately under this term. Has to match the
-- places in the syntax that get a call to `calcImports` in `pretty0`.
-- AST nodes that do a calcImports in pretty0, in order to try and emit a `use`
-- statement, need to be emitted also by this function, otherwise the `use`
-- statement may come out at an enclosing scope instead.
immediateChildBlockTerms ::
  forall a ap at v vt. (Var vt, Var v) => Term2 vt at ap v a -> [Term2 vt at ap v a]
immediateChildBlockTerms = \case
  LetBlock bs e -> concatMap doLet bs ++ handleDelay e
  _ -> []
  where
    handleDelay (Delay' b) | isLet b = [b]
    handleDelay _ = []
    doLet :: LetBindings v (Term2 vt at ap v a) -> [Term2 vt at ap v a]
    doLet = \case
      LetBindings bindings -> concatMap doLet2 bindings
      LetrecBindings bindings -> concatMap doLet2 bindings
    doLet2 (v, Ann' tm _) = doLet2 (v, tm)
    -- we don't consider 'body' to be a place we can insert a `use`
    -- clause unless it's already a let block. This avoids silliness like:
    --   x = 1 + 1
    -- turning into
    --   x =
    --    use Nat +
    --    1 + 1
    doLet2 (v, LamsNamedOpt' _ body) = [body | not (Var.isAction v), isLet body]
    doLet2 t = error (show t) []

isSoftHangable :: (Var v) => Term2 vt at ap v a -> Bool
-- isSoftHangable (Delay' d) = isLet d || isSoftHangable d || case d of
--    Match' scrute cases -> isDestructuringBind scrute cases
--    _ -> False
isSoftHangable (Delay' _) = True --
isSoftHangable (LamsNamedMatch' [] _) = True
isSoftHangable (Match' scrute cases) = not (isDestructuringBind scrute cases)
isSoftHangable _ = False

isLet :: Term2 vt at ap v a -> Bool
isLet (Let1Named' {}) = True
isLet (LetRecNamed' {}) = True
isLet _ = False

-- Matches with a single case, no variable shadowing, and where the pattern
-- has no literals are treated as destructuring bind, for instance:
--   match blah with (x,y) -> body
-- BECOMES
--   (x,y) = blah
--   body
-- BUT
--   match (y,x) with (x,y) -> body
-- Has shadowing, is rendered as a regular `match`.
--   match blah with 42 -> body
-- Pattern has (is) a literal, rendered as a regular match (rather than `42 = blah; body`)
isDestructuringBind :: (Ord v) => ABT.Term f v a -> [MatchCase loc (ABT.Term f v a)] -> Bool
isDestructuringBind scrutinee [MatchCase pat _ (ABT.AbsN' vs _)] =
  all (`Set.notMember` ABT.freeVars scrutinee) vs && not (hasLiteral pat)
  where
    hasLiteral p = case p of
      Pattern.Int _ _ -> True
      Pattern.Boolean _ _ -> True
      Pattern.Nat _ _ -> True
      Pattern.Float _ _ -> True
      Pattern.Text _ _ -> True
      Pattern.Char _ _ -> True
      Pattern.Constructor _ _ ps -> any hasLiteral ps
      Pattern.As _ p -> hasLiteral p
      Pattern.EffectPure _ p -> hasLiteral p
      Pattern.EffectBind _ _ ps pk -> any hasLiteral (pk : ps)
      Pattern.SequenceLiteral _ ps -> any hasLiteral ps
      Pattern.SequenceOp _ p _ p2 -> hasLiteral p || hasLiteral p2
      Pattern.Var _ -> False
      Pattern.Unbound _ -> False
isDestructuringBind _ _ = False

isBlock :: (Var v, Ord v) => Term2 vt at ap v a -> Bool
isBlock tm =
  case tm of
    If' {} -> True
    Handle' _ _ -> True
    Match' _ _ -> True
    LetBlock _ _ -> True
    DDelay' _ -> True
    Delay' _ -> True
    _ -> False

-- A `LetBindings` is either:
--

-- * A list of nonrecusrive lets (e.g. let x = ... in let y = ... in let z = ... in ...), where each binding is in

--   scope for all subsequent bindings.
--
--   In made-up syntax:
--
--     let
--       x = ...
--     in
--       let
--         y = ...
--       in
--         let
--           z = ...
--         in
--           body
--

-- * A single letrec's bindings, where each binding is in scope for all subsequent bindings.

--
--   In made-up syntax:
--
--     letrec
--       x = ...
--       y = ...
--       z = ...
--     in
--       body
data LetBindings v term
  = LetBindings [(v, term)]
  | LetrecBindings [(v, term)]

-- | A group of let bindings (with all bound variables cached at the top level for efficiency).
--
-- The sequence has an invariant: no two `LetBindings` in a row (that would be a single `LetBindings`).
--
-- For example, the bindings
--
--   a = ...
--   b = ...
--   c = ...
--   d = ...
--   e = ...
--   f = ...
--   body
--
-- might be two lets `a` and `b`, followed by a letrec `c` and `d`, followed by a different letrec `e`, `f`:
--
--   let
--     a = ...
--   in
--     let
--       b = ...
--     in
--       letrec
--         c = ...
--         d = ...
--       in
--         letrec
--           e = ...
--           f = ...
--         in
--           body
data LetBindingsGroups v term
  = LetBindingsGroups (Set v) (Seq (LetBindings v term))

instance (Ord v) => Semigroup (LetBindingsGroups v term) where
  LetBindingsGroups vs1 bs1 <> LetBindingsGroups vs2 bs2 =
    LetBindingsGroups (Set.union vs1 vs2) (bs1 <> bs2)

letBindingsToLetBindingsGroups :: (Ord v) => [(v, term)] -> LetBindingsGroups v term
letBindingsToLetBindingsGroups bindings =
  LetBindingsGroups (Set.fromList (map fst bindings)) (Seq.singleton (LetBindings bindings))

letrecBindingsToLetBindingsGroups :: (Ord v) => [(v, term)] -> LetBindingsGroups v term
letrecBindingsToLetBindingsGroups bindings =
  LetBindingsGroups (Set.fromList (map fst bindings)) (Seq.singleton (LetrecBindings bindings))

pattern LetBlock ::
  (Ord v) =>
  [LetBindings v (Term2 vt at ap v a)] ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a
pattern LetBlock bindings body <-
  (unLetBlock -> Just (LetBindingsGroups _ (Foldable.toList @Seq -> bindings), body))

-- Collects nested let/let rec blocks into one minimally nested block.
-- Handy because `let` and `let rec` blocks get rendered the same way.
-- We preserve nesting when the inner block shadows definitions in the
-- outer block.
unLetBlock ::
  forall a ap at v vt.
  (Ord v) =>
  Term2 vt at ap v a ->
  Maybe (LetBindingsGroups v (Term2 vt at ap v a), Term2 vt at ap v a)
unLetBlock = rec
  where
    dontIntersect :: LetBindingsGroups v term -> LetBindingsGroups v term -> Bool
    dontIntersect (LetBindingsGroups xs _) (LetBindingsGroups ys _) =
      Set.disjoint xs ys

    rec :: Term2 vt at ap v a -> Maybe (LetBindingsGroups v (Term2 vt at ap v a), Term2 vt at ap v a)
    rec t = case unLetRecNamed t of
      Nothing -> nonrec t
      Just (_isTop, bindings0, body) ->
        let bindings = letrecBindingsToLetBindingsGroups bindings0
         in case rec body of
              Just (innerBindings, innerBody)
                | dontIntersect bindings innerBindings ->
                    Just (bindings <> innerBindings, innerBody)
              _ -> Just (bindings, body)

    nonrec :: Term2 vt at ap v a -> Maybe (LetBindingsGroups v (Term2 vt at ap v a), Term2 vt at ap v a)
    nonrec t = case unLet t of
      Nothing -> Nothing
      Just (bindings0, body) ->
        let bindings = letBindingsToLetBindingsGroups [(v, b) | (_, v, b) <- bindings0]
         in case rec body of
              Just (innerBindings, innerBody)
                | dontIntersect bindings innerBindings ->
                    Just (bindings <> innerBindings, innerBody)
              _ -> Just (bindings, body)

pattern LamsNamedMatch' ::
  (Var v) =>
  [v] ->
  [([Pattern ap], Maybe (Term2 vt at ap v a), Term2 vt at ap v a)] ->
  Term2 vt at ap v a
pattern LamsNamedMatch' vs branches <- (unLamsMatch' -> Just (vs, branches))

-- This function is used to detect places where lambda case syntax can be used.
-- When given lambdas of a form that corresponds to a lambda case, it returns
-- `Just (varsBeforeCases, branches)`. Leading vars are the vars that should be
-- shown to the left of the `-> cases`.
--
-- For instance, if given this term:
--
--   x y z -> match z with
--     [] -> "empty"
--     (h +: t) -> "nonempty"
--
-- this function will return Just ([x,y], [[] -> "empty", (h +: t) -> "nonempty"])
-- and it would be rendered as
--
--   x y -> cases []     -> "empty"
--                h +: t -> "nonempty"
--
-- Given this term
--
--   x y z -> match (y, z) with
--     ("a", "b") -> "abba"
--     (x, y) -> y ++ x
--
-- this function will return Just ([x], [ "a" "b" -> "abba", x y -> y ++ x])
-- and it would be rendered as `x -> cases "a", "b" -> "abba"
--                                         x,    y  -> y ++ x
--
-- This function returns `Nothing` in cases where the term it is given isn't
-- a lambda, or when the lambda isn't in the correct form for lambda cases.
-- (For instance, `x -> match (x, 42) with ...` can't be written using
-- lambda case)
unLamsMatch' ::
  (Var v) =>
  Term2 vt at ap v a ->
  Maybe ([v], [([Pattern ap], Maybe (Term2 vt at ap v a), Term2 vt at ap v a)])
unLamsMatch' t = case unLamsUntilDelay' t of
  -- x -> match x with pat -> ...
  --   becomes
  -- cases pat -> ...
  Just (reverse -> (v1 : vs), Match' (Var' v1') branches)
    | -- if `v1'` is referenced in any of the branches, we can't use lambda case
      -- syntax as we need to keep the `v1'` name that was introduced
      v1 == v1' && Set.notMember v1' (Set.unions $ freeVars <$> branches) ->
        Just (reverse vs, [([p], guard, body) | MatchCase p guard body <- branches])
  -- x y z -> match (x,y,z) with (pat1, pat2, pat3) -> ...
  --   becomes
  -- cases pat1 pat2 pat3 -> ...`
  Just (reverse -> vs@(_ : _), Match' (TupleTerm' scrutes) branches)
    | multiway vs (reverse scrutes)
        &&
        -- (as above) if any of the vars are referenced in any of the branches,
        -- we need to keep the names introduced by the lambda and can't use
        -- lambda case syntax
        all notFree (take len vs)
        && all isRightArity branches
        && len /= 0 -> -- all patterns need to match arity of scrutes
        Just (reverse (drop len vs), branches')
    where
      isRightArity (MatchCase (TuplePattern ps) _ _) = length ps == len
      isRightArity MatchCase {} = False
      len = length scrutes
      fvs = Set.unions $ freeVars <$> branches
      notFree v = Set.notMember v fvs
      branches' = [(ps, guard, body) | MatchCase (TuplePattern ps) guard body <- branches]
  _ -> Nothing
  where
    -- multiway vs tms checks that length tms <= length vs, and their common prefix
    -- is all matching variables
    multiway _ [] = True
    multiway (h : t) (Var' h2 : t2) | h == h2 = multiway t t2
    multiway _ _ = False
    freeVars (MatchCase _ g rhs) =
      let guardVars = maybe Set.empty ABT.freeVars g
          rhsVars = ABT.freeVars rhs
       in Set.union guardVars rhsVars

pattern Bytes' :: [Word64] -> Term3 v PrintAnnotation
pattern Bytes' bs <- (toBytes -> Just bs)

toBytes :: Term3 v PrintAnnotation -> Maybe [Word64]
toBytes (App' (Builtin' "Bytes.fromList") (List' bs)) =
  toList <$> traverse go bs
  where
    go (Nat' n) = Just n
    go _ = Nothing
toBytes _ = Nothing

prettyDoc2 ::
  forall v m.
  (MonadPretty v m) =>
  AmbientContext ->
  Term3 v PrintAnnotation ->
  m (Maybe (Pretty SyntaxText))
prettyDoc2 ac tm = do
  env <- ask
  let brace p =
        if PP.isMultiLine p
          then fmt S.DocDelimiter "{{" <> PP.newline <> p <> PP.newline <> fmt S.DocDelimiter "}}"
          else fmt S.DocDelimiter "{{" <> PP.softbreak <> p <> PP.softbreak <> fmt S.DocDelimiter "}}"
      bail tm = brace <$> pretty0 ac tm
      contains :: Char -> Pretty SyntaxText -> Bool
      contains c p =
        PP.toPlainUnbroken (PP.syntaxToColor p)
          & elem c
      -- Finds the longest run of a character and return one bigger than that
      longestRun c s =
        case filter (\s -> take 2 s == [c, c]) $
          List.group (PP.toPlainUnbroken $ PP.syntaxToColor s) of
          [] -> 2
          x -> 1 + maximum (map length x)
      oneMore c inner = replicate (longestRun c inner) c
      makeFence inner = PP.string $ replicate (max 3 $ longestRun '`' inner) '`'
      go :: Width -> Term3 v PrintAnnotation -> m (Pretty SyntaxText)
      go hdr = \case
        (toDocTransclude env.ppe -> Just d) ->
          bail d
        (toDocUntitledSection env.ppe -> Just ds) ->
          sepBlankline ds
        (toDocSection env.ppe -> Just (title, ds)) -> do
          prettyTitle <- rec title
          prettyDs <- intercalateMapM "\n\n" (go (hdr + 1)) ds
          pure $
            PP.lines
              [ PP.text (Text.replicate (PP.widthToInt hdr) "#") <> " " <> prettyTitle,
                "",
                PP.indentN (hdr + 1) prettyDs
              ]
        (toDocParagraph env.ppe -> Just ds) ->
          PP.wrap . mconcat <$> traverse rec ds
        (toDocBulletedList env.ppe -> Just ds) -> do
          PP.lines <$> traverse item ds
          where
            item d = ("* " <>) . PP.indentAfterNewline "  " <$> rec d
        (toDocNumberedList env.ppe -> Just (n, ds)) ->
          PP.column2 <$> traverse item (zip [n ..] ds)
          where
            item (n, d) = (PP.group (PP.shown n <> "."),) <$> rec d
        (toDocWord env.ppe -> Just t) ->
          pure $ PP.text t
        (toDocCode env.ppe -> Just d) -> do
          inner <- rec d
          let quotes =
                -- Prefer ` if there aren't any in the inner text,
                -- otherwise use one more than the longest run of ' in the inner text
                if contains '`' inner
                  then PP.string $ oneMore '\'' inner
                  else PP.string "`"
          pure $ PP.group $ quotes <> inner <> quotes
        (toDocJoin env.ppe -> Just ds) -> foldMapM rec ds
        (toDocItalic env.ppe -> Just d) -> do
          inner <- rec d
          let underscores = PP.string $ oneMore '_' inner
          pure $ PP.group $ underscores <> inner <> underscores
        (toDocBold env.ppe -> Just d) -> do
          inner <- rec d
          let stars = PP.string $ oneMore '*' inner
          pure $ PP.group $ stars <> inner <> stars
        (toDocStrikethrough env.ppe -> Just d) -> do
          inner <- rec d
          let quotes = PP.string $ oneMore '~' inner
          pure $ PP.group $ quotes <> inner <> quotes
        (toDocGroup env.ppe -> Just d) ->
          PP.group <$> rec d
        (toDocColumn env.ppe -> Just ds) ->
          PP.lines <$> traverse rec ds
        (toDocNamedLink env.ppe -> Just (name, target)) ->
          do
            name' <- rec name
            target' <- rec target
            pure $ PP.group $ "[" <> name' <> "](" <> target' <> ")"
        (toDocLink env.ppe -> Just e) -> pure . PP.group $ case e of
          Left r -> "{type " <> tyName r <> "}"
          Right r -> "{" <> tmName r <> "}"
        (toDocEval env.ppe -> Just tm) ->
          do
            inner <- pretty0 ac tm
            let fence = makeFence inner
            pure $ PP.lines [fence, inner, fence]
        (toDocEvalInline env.ppe -> Just tm) ->
          do
            inner <- pretty0 ac tm
            pure $ "@eval{" <> inner <> "}"
        (toDocExample env.ppe -> Just tm) ->
          do
            inner <- pretty0 ac tm
            pure $ "``" <> inner <> "``"
        (toDocExampleBlock env.ppe -> Just tm) ->
          do
            inner <- pretty0 ac' tm
            let fence = makeFence inner
            pure $ PP.lines ["@typecheck " <> fence, inner, fence]
          where
            ac' = ac {elideUnit = True}
        (toDocSource env.ppe -> Just es) ->
          pure . PP.group $ "    @source{" <> intercalateMap ", " go es <> "}"
          where
            go (Left r, _anns) = "type " <> tyName r
            go (Right r, _anns) = tmName r
        (toDocFoldedSource env.ppe -> Just es) ->
          pure . PP.group $ "    @foldedSource{" <> intercalateMap ", " go es <> "}"
          where
            go (Left r, _anns) = "type " <> tyName r
            go (Right r, _anns) = tmName r
        (toDocSignatureInline env.ppe -> Just tm) ->
          pure . PP.group $ "@inlineSignature{" <> tmName tm <> "}"
        (toDocSignature env.ppe -> Just tms) ->
          let name = if length tms == 1 then "@signature" else "@signatures"
           in pure . PP.group $ "    " <> name <> "{" <> intercalateMap ", " tmName tms <> "}"
        (toDocCodeBlock env.ppe -> Just (typ, txt)) ->
          pure $
            let txt' = PP.text txt
                fence = makeFence txt'
             in PP.group $
                  PP.lines
                    [ fence <> " " <> PP.text typ,
                      PP.group txt',
                      fence
                    ]
        (toDocVerbatim env.ppe -> Just txt) ->
          pure $
            PP.group $
              PP.lines
                [ "'''",
                  PP.group $ PP.text txt,
                  "'''"
                ]
        -- todo : emit fewer gratuitous columns, maybe a wrapIfMany combinator
        tm -> bail tm
        where
          im = imports ac
          tyName r = styleHashQualified'' (fmt $ S.TypeReference r) . elideFQN im $ PrettyPrintEnv.typeName env.ppe r
          tmName r = styleHashQualified'' (fmt $ S.TermReference r) . elideFQN im $ PrettyPrintEnv.termName env.ppe r
          rec = go hdr
          sepBlankline = intercalateMapM "\n\n" rec
  case tm of
    -- these patterns can introduce a {{ .. }} block
    (toDocUntitledSection env.ppe -> Just _) -> Just . brace <$> go 1 tm
    (toDocSection env.ppe -> Just _) -> Just . brace <$> go 1 tm
    (toDocParagraph env.ppe -> Just _) -> Just . brace <$> go 1 tm
    _ -> pure Nothing

toDocJoin :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Term3 v PrintAnnotation]
toDocJoin ppe (App' (Ref' r) (List' tms))
  | nameEndsWith ppe ".docJoin" r = Just (toList tms)
toDocJoin _ _ = Nothing

toDocUntitledSection :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Term3 v PrintAnnotation]
toDocUntitledSection ppe (App' (Ref' r) (List' tms))
  | nameEndsWith ppe ".docUntitledSection" r = Just (toList tms)
toDocUntitledSection _ _ = Nothing

toDocColumn :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Term3 v PrintAnnotation]
toDocColumn ppe (App' (Ref' r) (List' tms))
  | nameEndsWith ppe ".docColumn" r = Just (toList tms)
toDocColumn _ _ = Nothing

toDocGroup :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocGroup ppe (App' (Ref' r) doc)
  | nameEndsWith ppe ".docGroup" r = Just doc
toDocGroup _ _ = Nothing

toDocWord :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe Text
toDocWord ppe (App' (Ref' r) (Text' txt))
  | nameEndsWith ppe ".docWord" r = Just txt
toDocWord _ _ = Nothing

toDocBold :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocBold ppe (App' (Ref' r) doc)
  | nameEndsWith ppe ".docBold" r = Just doc
toDocBold _ _ = Nothing

toDocCode :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocCode ppe (App' (Ref' r) doc)
  | nameEndsWith ppe ".docCode" r = Just doc
toDocCode _ _ = Nothing

toDocCodeBlock :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Text, Text)
toDocCodeBlock ppe (Apps' (Ref' r) [Text' typ, Text' txt])
  | nameEndsWith ppe ".docCodeBlock" r = Just (typ, txt)
toDocCodeBlock _ _ = Nothing

toDocVerbatim :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe Text
toDocVerbatim ppe (App' (Ref' r) (toDocWord ppe -> Just txt))
  | nameEndsWith ppe ".docVerbatim" r = Just txt
toDocVerbatim _ _ = Nothing

toDocEval :: (Var v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocEval ppe (App' (Ref' r) (DDelay' tm))
  | nameEndsWith ppe ".docEval" r = Just tm
  | r == _oldDocEval = Just tm
toDocEval _ _ = Nothing

-- Old hashes for docEval, docEvalInline w/ incorrect type signatures.
-- They are still used by some existing docs so the pretty-printer
-- recognizes it.
--
-- See https://github.com/unisonweb/unison/issues/2238
_oldDocEval, _oldDocEvalInline :: Reference
_oldDocEval = Reference.unsafeFromText "#m2bmkdos2669tt46sh2gf6cmb4td5le8lcqnmsl9nfaqiv7s816q8bdtjdbt98tkk11ejlesepe7p7u8p0asu9758gdseffh0t78m2o"
_oldDocEvalInline = Reference.unsafeFromText "#7pjlvdu42gmfvfntja265dmi08afk08l54kpsuu55l9hq4l32fco2jlrm8mf2jbn61esfsi972b6e66d9on4i5bkmfchjdare1v5npg"

-- for docs, we consider a delay to be any function that ignores its arg
pattern DDelay' :: (Var v) => Term2 vt at ap v a -> Term2 vt at ap v a
pattern DDelay' body <- (unDDelay -> Just body)

unDDelay :: (Var v) => Term2 vt at ap v a -> Maybe (Term2 vt at ap v a)
unDDelay tm = case ABT.out tm of
  ABT.Tm (Lam (ABT.Term _ _ (ABT.Abs v body)))
    | Set.notMember v (ABT.freeVars body) -> Just body
  _ -> Nothing

toDocEvalInline :: (Var v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocEvalInline ppe (App' (Ref' r) (DDelay' tm))
  | nameEndsWith ppe ".docEvalInline" r = Just tm
  | r == _oldDocEvalInline = Just tm
toDocEvalInline _ _ = Nothing

toDocExample, toDocExampleBlock :: (Var v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocExample = toDocExample' ".docExample"
toDocExampleBlock = toDocExample' ".docExampleBlock"

toDocExample' :: (Var v) => Text -> PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocExample' suffix ppe (Apps' (Ref' r) [Nat' n, l@(LamsNamed' vs tm)])
  | nameEndsWith ppe suffix r,
    ABT.freeVars l == mempty,
    ok tm =
      Just (lamWithoutBindingAnns (ABT.annotation l) (drop (fromIntegral n + 1) vs) tm)
  where
    ok (Apps' f _) = ABT.freeVars f == mempty
    ok tm = ABT.freeVars tm == mempty
toDocExample' _ _ _ = Nothing

toDocTransclude :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocTransclude ppe (App' (Ref' r) tm)
  | nameEndsWith ppe ".docTransclude" r = Just tm
toDocTransclude _ _ = Nothing

toDocLink :: (Var v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Either Reference Referent)
toDocLink ppe (App' (Ref' r) tm)
  | nameEndsWith ppe ".docLink" r = case tm of
      (toDocEmbedTermLink ppe -> Just tm) -> Just (Right tm)
      (toDocEmbedTypeLink ppe -> Just tm) -> Just (Left tm)
      _ -> Nothing
toDocLink _ _ = Nothing

toDocNamedLink :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation, Term3 v PrintAnnotation)
toDocNamedLink ppe (Apps' (Ref' r) [name, target])
  | nameEndsWith ppe ".docNamedLink" r = Just (name, target)
toDocNamedLink _ _ = Nothing

toDocItalic :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocItalic ppe (App' (Ref' r) doc)
  | nameEndsWith ppe ".docItalic" r = Just doc
toDocItalic _ _ = Nothing

toDocStrikethrough :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocStrikethrough ppe (App' (Ref' r) doc)
  | nameEndsWith ppe ".docStrikethrough" r = Just doc
toDocStrikethrough _ _ = Nothing

toDocParagraph :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Term3 v PrintAnnotation]
toDocParagraph ppe (App' (Ref' r) (List' tms))
  | nameEndsWith ppe ".docParagraph" r = Just (toList tms)
toDocParagraph _ _ = Nothing

toDocEmbedTermLink :: (Var v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe Referent
toDocEmbedTermLink ppe (App' (Ref' r) (DDelay' (Referent' tm)))
  | nameEndsWith ppe ".docEmbedTermLink" r = Just tm
toDocEmbedTermLink _ _ = Nothing

toDocEmbedTypeLink :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe Reference
toDocEmbedTypeLink ppe (App' (Ref' r) (TypeLink' typeref))
  | nameEndsWith ppe ".docEmbedTypeLink" r = Just typeref
toDocEmbedTypeLink _ _ = Nothing

toDocSourceAnnotations :: (Var v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Referent]
toDocSourceAnnotations _ppe _tm = Just [] -- todo fetch annotations

toDocSourceElement :: (Var v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Either Reference Referent, [Referent])
toDocSourceElement ppe (Apps' (Ref' r) [tm, toDocSourceAnnotations ppe -> Just annotations])
  | nameEndsWith ppe ".docSourceElement" r =
      (,annotations) <$> ok tm
  where
    ok tm =
      Right <$> toDocEmbedTermLink ppe tm
        <|> Left <$> toDocEmbedTypeLink ppe tm
toDocSourceElement _ _ = Nothing

toDocSource' ::
  (Var v) =>
  Text ->
  PrettyPrintEnv ->
  Term3 v PrintAnnotation ->
  Maybe [(Either Reference Referent, [Referent])]
toDocSource' suffix ppe (App' (Ref' r) (List' tms))
  | nameEndsWith ppe suffix r =
      case [tm | Just tm <- toDocSourceElement ppe <$> toList tms] of
        tms' | length tms' == length tms -> Just tms'
        _ -> Nothing
toDocSource' _ _ _ = Nothing

toDocSource,
  toDocFoldedSource ::
    (Var v) =>
    PrettyPrintEnv ->
    Term3 v PrintAnnotation ->
    Maybe [(Either Reference Referent, [Referent])]
toDocSource = toDocSource' ".docSource"
toDocFoldedSource = toDocSource' ".docFoldedSource"

toDocSignatureInline :: (Var v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe Referent
toDocSignatureInline ppe (App' (Ref' r) (toDocEmbedSignatureLink ppe -> Just tm))
  | nameEndsWith ppe ".docSignatureInline" r = Just tm
toDocSignatureInline _ _ = Nothing

toDocEmbedSignatureLink :: (Var v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe Referent
toDocEmbedSignatureLink ppe (App' (Ref' r) (DDelay' (Referent' tm)))
  | nameEndsWith ppe ".docEmbedSignatureLink" r = Just tm
toDocEmbedSignatureLink _ _ = Nothing

-- toDocEmbedAnnotation :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
-- toDocEmbedAnnotation ppe (App' (Ref' r) tm)
--   | nameEndsWith ppe ".docEmbedAnnotation" r = Just tm
-- toDocEmbedAnnotation _ _ = Nothing

-- toDocEmbedAnnotations :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Term3 v PrintAnnotation]
-- toDocEmbedAnnotations ppe (App' (Ref' r) (List' tms))
--   | nameEndsWith ppe ".docEmbedAnnotations" r =
--     case [ann | Just ann <- toDocEmbedAnnotation ppe <$> toList tms] of
--       tms' | length tms' == length tms -> Just tms'
--       _ -> Nothing
-- toDocEmbedAnnotations _ _ = Nothing

toDocSignature :: (Var v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Referent]
toDocSignature ppe (App' (Ref' r) (List' tms))
  | nameEndsWith ppe ".docSignature" r =
      case [tm | Just tm <- toDocEmbedSignatureLink ppe <$> toList tms] of
        tms' | length tms' == length tms -> Just tms'
        _ -> Nothing
toDocSignature _ _ = Nothing

toDocBulletedList :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Term3 v PrintAnnotation]
toDocBulletedList ppe (App' (Ref' r) (List' tms))
  | nameEndsWith ppe ".docBulletedList" r = Just (toList tms)
toDocBulletedList _ _ = Nothing

toDocNumberedList ::
  PrettyPrintEnv ->
  Term3 v PrintAnnotation ->
  Maybe (Word64, [Term3 v PrintAnnotation])
toDocNumberedList ppe (Apps' (Ref' r) [Nat' n, List' tms])
  | nameEndsWith ppe ".docNumberedList" r = Just (n, toList tms)
toDocNumberedList _ _ = Nothing

toDocSection ::
  PrettyPrintEnv ->
  Term3 v PrintAnnotation ->
  Maybe (Term3 v PrintAnnotation, [Term3 v PrintAnnotation])
toDocSection ppe (Apps' (Ref' r) [title, List' tms])
  | nameEndsWith ppe ".docSection" r = Just (title, toList tms)
toDocSection _ _ = Nothing

nameEndsWith :: PrettyPrintEnv -> Text -> Reference -> Bool
nameEndsWith ppe suffix r = case PrettyPrintEnv.termName ppe (Referent.Ref r) of
  HQ.NameOnly n ->
    let tn = Name.toText n
     in tn == Text.drop 1 suffix || Text.isSuffixOf suffix tn
  _ -> False

-- Modifies a PrettyPrintEnv to avoid picking a name for a term or type ref
-- which is the same as a locally introduced variable. For example:
--
--   qux.quaffle = 23
--
--   example : Text -> Nat
--   example quaffle = qux.quaffle + 1
--
-- Here, we want the pretty-printer to use the name 'qux.quaffle' even though
-- 'quaffle' would otherwise be a unique suffix.
--
-- Algorithm is the following:
--   1. Form the set of all local variables used anywhere in the term
--   2. When picking a name for a term, see if it is contained in this set.
--      If yes: use a minimally qualified name which is longer than the suffixed name,
--              but doesn't conflict with any local vars. If even the fully-qualified
--              name conflicts with any local vars, make it absolute. (This relies on
--              disallowing absolute names for local variables).
--      If no: use the suffixed name for the term
--
-- The algorithm does the same for type references in signatures.
--
-- This algorithm is conservative in the sense that it doesn't take into account
-- the binding structure of the term. If the variable 'quaffle' is used as a local
-- variable anywhere in the term, then 'quaffle' will not be considered a unique suffix
-- even in places where the local 'quaffle' isn't in scope.
--
-- To do better this, you'd have to track bound variables in the pretty-printer and
-- fold this logic into the core pretty-printing implementation. This conservative
-- algorithm has the advantage of being purely a preprocessing step.
avoidShadowing :: (Var v, Var vt) => Term2 vt at ap v a -> PrettyPrintEnv -> PrettyPrintEnv
avoidShadowing tm (PrettyPrintEnv terms types) =
  PrettyPrintEnv terms' types'
  where
    terms' r = tweak usedTermNames <$> terms r
    types' r = tweak usedTypeNames <$> types r
    usedTermNames =
      Set.fromList [n | v <- ABT.allVars tm, n <- varToName v]
    usedTypeNames =
      Set.fromList [n | Ann' _ ty <- ABT.subterms tm, v <- ABT.allVars ty, n <- varToName v]
    tweak :: Set Name -> (HQ'.HashQualified Name, HQ'.HashQualified Name) -> (HQ'.HashQualified Name, HQ'.HashQualified Name)
    tweak used (HQ'.NameOnly fullName, HQ'.NameOnly suffixedName)
      | Set.member suffixedName used =
          let resuffixifiedName :: Name
              resuffixifiedName =
                fullName
                  & Name.suffixes
                  -- Drop the suffixes that we know are shorter than the suffixified name
                  & List.drop (Name.countSegments suffixedName)
                  -- Find the first (shortest) suffix that isn't in the used set
                  & find (\n -> n `Set.notMember` used)
                  -- If there isn't one, use the absolut-ified full name
                  & fromMaybe (Name.makeAbsolute fullName)
           in (HQ'.NameOnly fullName, HQ'.NameOnly resuffixifiedName)
    tweak _ p = p
    varToName :: (Var v) => v -> [Name]
    varToName = toList . Name.parseText . Var.name . Var.reset

isLeaf :: Term2 vt at ap v a -> Bool
isLeaf (Var' {}) = True
isLeaf (Constructor' {}) = True
isLeaf (Request' {}) = True
isLeaf (Ref' {}) = True
isLeaf _ = False
