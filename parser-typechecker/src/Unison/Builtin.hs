{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Unison.Builtin
  (codeLookup
  ,constructorType
  ,names
  ,names0
  ,builtinDataDecls
  ,builtinEffectDecls
  ,builtinConstructorType
  ,builtinTypeDependents
  ,builtinTypeDependentsOfComponent
  ,builtinTypes
  ,builtinTermsByType
  ,builtinTermsByTypeMention
  ,intrinsicTermReferences
  ,intrinsicTypeReferences
  ,isBuiltinType
  ,typeOf
  ,typeLookup
  ,termRefTypes
  ) where

import Unison.Prelude

import           Data.Bifunctor                 ( second, first )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import qualified Text.Regex.TDFA               as RE
import Unison.ConstructorReference (GConstructorReference(..))
import qualified Unison.ConstructorType        as CT
import           Unison.Codebase.CodeLookup     ( CodeLookup(..) )
import qualified Unison.Builtin.Decls          as DD
import qualified Unison.Builtin.Terms          as TD
import qualified Unison.DataDeclaration        as DD
import Unison.Hash (Hash)
import Unison.Parser.Ann (Ann (..))
import qualified Unison.Reference              as R
import qualified Unison.Referent               as Referent
import           Unison.Symbol                  ( Symbol )
import qualified Unison.Type                   as Type
import qualified Unison.Var                    as Var
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name
import           Unison.NamesWithHistory (NamesWithHistory(..))
import           Unison.Names            (Names (Names))
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.Util.Relation          as Rel
import qualified Unison.Hashing.V2.Convert as H

type DataDeclaration = DD.DataDeclaration Symbol Ann
type EffectDeclaration = DD.EffectDeclaration Symbol Ann
type Type = Type.Type Symbol ()

names :: NamesWithHistory
names = NamesWithHistory names0 mempty

names0 :: Names
names0 = Names terms types where
  terms = Rel.mapRan Referent.Ref (Rel.fromMap termNameRefs) <>
    Rel.fromList [ (Name.unsafeFromVar vc, Referent.Con (ConstructorReference (R.DerivedId r) cid) ct)
                 | (ct, (_,(r,decl))) <- ((CT.Data,) <$> builtinDataDecls) <>
                    ((CT.Effect,) . (second . second) DD.toDataDecl <$> builtinEffectDecls)
                 , ((_,vc,_), cid) <- DD.constructors' decl `zip` [0..]] <>
    Rel.fromList [ (Name.unsafeFromVar v, Referent.Ref (R.DerivedId i))
                 | (v,i) <- Map.toList $ TD.builtinTermsRef]
  types = Rel.fromList builtinTypes <>
    Rel.fromList [ (Name.unsafeFromVar v, R.DerivedId r)
                 | (v,(r,_)) <- builtinDataDecls ] <>
    Rel.fromList [ (Name.unsafeFromVar v, R.DerivedId r)
                 | (v,(r,_)) <- builtinEffectDecls ]

-- note: this function is really for deciding whether `r` is a term or type,
-- but it can only answer correctly for Builtins.
isBuiltinType :: R.Reference -> Bool
isBuiltinType r = elem r . fmap snd $ builtinTypes

typeLookup :: TL.TypeLookup Symbol Ann
typeLookup =
  TL.TypeLookup
    (fmap (const Intrinsic) <$> termRefTypes)
    (Map.fromList . map (first R.DerivedId) $ map snd builtinDataDecls)
    (Map.fromList . map (first R.DerivedId) $ map snd builtinEffectDecls)

constructorType :: R.Reference -> Maybe CT.ConstructorType
constructorType r = TL.constructorType typeLookup r
                <|> Map.lookup r builtinConstructorType

builtinDataDecls :: [(Symbol, (R.Id, DataDeclaration))]
builtinDataDecls =
  [ (v, (r, Intrinsic <$ d)) | (v, r, d) <- DD.builtinDataDecls ]

builtinEffectDecls :: [(Symbol, (R.Id, EffectDeclaration))]
builtinEffectDecls = [ (v, (r, Intrinsic <$ d)) | (v, r, d) <- DD.builtinEffectDecls ]

codeLookup :: Applicative m => CodeLookup Symbol m Ann
codeLookup = CodeLookup (const $ pure Nothing) $ \r ->
  pure
    $ lookup r [ (r, Right x) | (r, x) <- snd <$> builtinDataDecls ]
  <|> lookup r [ (r, Left x)  | (r, x) <- snd <$> builtinEffectDecls ]

-- Relation predicate: Domain depends on range.
builtinDependencies :: Rel.Relation R.Reference R.Reference
builtinDependencies =
  Rel.fromMultimap (Type.dependencies <$> termRefTypes)

-- a relation whose domain is types and whose range is builtin terms with that type
builtinTermsByType :: Rel.Relation R.Reference Referent.Referent
builtinTermsByType =
  Rel.fromList [ (H.typeToReference ty, Referent.Ref r)
               | (r, ty) <- Map.toList termRefTypes ]

-- a relation whose domain is types and whose range is builtin terms that mention that type
-- example: Nat.+ mentions the type `Nat`
builtinTermsByTypeMention :: Rel.Relation R.Reference Referent.Referent
builtinTermsByTypeMention =
  Rel.fromList [ (m, Referent.Ref r) | (r, ty) <- Map.toList termRefTypes
                                     , m <- toList $ H.typeToReferenceMentions ty ]

-- The dependents of a builtin type is the set of builtin terms which
-- mention that type.
builtinTypeDependents :: R.Reference -> Set R.Reference
builtinTypeDependents r = Rel.lookupRan r builtinDependencies

builtinTypeDependentsOfComponent :: Hash -> Set R.Reference
builtinTypeDependentsOfComponent h0 = Rel.searchRan ord builtinDependencies
  where
    ord :: R.Reference -> Ordering
    ord = \case
      R.Derived h _i -> compare h h0
      r -> compare r r0
    r0 = R.Derived h0 0

-- WARNING:
-- As with the terms, we should avoid changing these references, even
-- if we decide to change their names.
builtinTypes :: [(Name, R.Reference)]
builtinTypes = Map.toList . Map.mapKeys Name.unsafeFromText
                          $ foldl' go mempty builtinTypesSrc where
  go m = \case
    B' r _ -> Map.insert r (R.Builtin r) m
    D' r -> Map.insert r (R.Builtin r) m
    Rename' r name -> case Map.lookup name m of
      Just _ -> error . Text.unpack $
                "tried to rename `" <> r <> "` to `" <> name <> "`, " <>
                "which already exists."
      Nothing -> case Map.lookup r m of
        Nothing -> error . Text.unpack $
                "tried to rename `" <> r <> "` before it was declared."
        Just t -> Map.insert name t . Map.delete r $ m
    Alias' r name -> case Map.lookup name m of
      Just _ -> error . Text.unpack $
                "tried to alias `" <> r <> "` to `" <> name <> "`, " <>
                "which already exists."
      Nothing -> case Map.lookup r m of
        Nothing -> error . Text.unpack $
                  "tried to alias `" <> r <> "` before it was declared."
        Just t -> Map.insert name t m

-- WARNING: Don't delete any of these lines, only add corrections.
builtinTypesSrc :: [BuiltinTypeDSL]
builtinTypesSrc =
  [ B' "Int" CT.Data
  , B' "Nat" CT.Data
  , B' "Float" CT.Data
  , B' "Boolean" CT.Data
  , B' "Sequence" CT.Data, Rename' "Sequence" "List"
  , B' "Text" CT.Data
  , B' "Char" CT.Data
  , B' "Effect" CT.Data, Rename' "Effect" "Request"
  , B' "Bytes" CT.Data
  , B' "Link.Term" CT.Data
  , B' "Link.Type" CT.Data
  , B' "IO" CT.Effect, Rename' "IO" "io2.IO"
  , B' "Handle" CT.Data, Rename' "Handle" "io2.Handle"
  , B' "Socket" CT.Data, Rename' "Socket" "io2.Socket"
  , B' "ThreadId" CT.Data, Rename' "ThreadId" "io2.ThreadId"
  , B' "MVar" CT.Data, Rename' "MVar" "io2.MVar"
  , B' "Code" CT.Data
  , B' "Value" CT.Data
  , B' "Any" CT.Data
  , B' "crypto.HashAlgorithm" CT.Data
  , B' "Tls" CT.Data, Rename' "Tls" "io2.Tls"
  , B' "Tls.ClientConfig" CT.Data, Rename' "Tls.ClientConfig" "io2.Tls.ClientConfig"
  , B' "Tls.ServerConfig" CT.Data, Rename' "Tls.ServerConfig" "io2.Tls.ServerConfig"
  , B' "Tls.SignedCert" CT.Data, Rename' "Tls.SignedCert" "io2.Tls.SignedCert"
  , B' "Tls.PrivateKey" CT.Data, Rename' "Tls.PrivateKey" "io2.Tls.PrivateKey"
  , B' "Tls.Version" CT.Data, Rename' "Tls.Version" "io2.Tls.Version"
  , B' "Tls.Cipher" CT.Data, Rename' "Tls.Cipher" "io2.Tls.Cipher"
  , B' "TVar" CT.Data, Rename' "TVar" "io2.TVar"
  , B' "STM" CT.Effect, Rename' "STM" "io2.STM"
  , B' "Ref" CT.Data
  , B' "Scope" CT.Effect
  ]

-- rename these to "builtin" later, when builtin means intrinsic as opposed to
-- stuff that intrinsics depend on.
intrinsicTypeReferences :: Set R.Reference
intrinsicTypeReferences = foldl' go mempty builtinTypesSrc where
  go acc = \case
    B' r _ -> Set.insert (R.Builtin r) acc
    D' r -> Set.insert (R.Builtin r) acc
    _ -> acc

intrinsicTermReferences :: Set R.Reference
intrinsicTermReferences = Map.keysSet termRefTypes

builtinConstructorType :: Map R.Reference CT.ConstructorType
builtinConstructorType = Map.fromList [ (R.Builtin r, ct) | B' r ct <- builtinTypesSrc ]

data BuiltinTypeDSL = B' Text CT.ConstructorType | D' Text | Rename' Text Text | Alias' Text Text


data BuiltinDSL
  -- simple builtin: name=ref, type
  = B Text Type
  -- deprecated builtin: name=ref, type (TBD)
  | D Text Type
  -- rename builtin: refname, newname
  -- must not appear before corresponding B/D
  -- will overwrite newname
  | Rename Text Text
  -- alias builtin: refname, newname
  -- must not appear before corresponding B/D
  -- will overwrite newname
  | Alias Text Text


instance Show BuiltinDSL where
  show (B t _) = Text.unpack $ "B" <> t
  show (Rename from to) = Text.unpack $ "Rename " <> from <> " to " <> to
  show _ = ""

termNameRefs :: Map Name R.Reference
termNameRefs = Map.mapKeys Name.unsafeFromText $ foldl' go mempty (stripVersion builtinsSrc) where
  go m = \case
    B r _tp -> Map.insert r (R.Builtin r) m
    D r _tp -> Map.insert r (R.Builtin r) m
    Rename r name -> case Map.lookup name m of
      Just _ -> error . Text.unpack $
                "tried to rename `" <> r <> "` to `" <> name <> "`, " <>
                "which already exists."
      Nothing -> case Map.lookup r m of
        Nothing -> error . Text.unpack $
                "tried to rename `" <> r <> "` before it was declared."
        Just t -> Map.insert name t . Map.delete r $ m
    Alias r name -> case Map.lookup name m of
      Just _ -> error . Text.unpack $
                "tried to alias `" <> r <> "` to `" <> name <> "`, " <>
                "which already exists."
      Nothing -> case Map.lookup r m of
        Nothing -> error . Text.unpack $
                  "tried to alias `" <> r <> "` before it was declared."
        Just t -> Map.insert name t m

termRefTypes :: Map R.Reference Type
termRefTypes = foldl' go mempty builtinsSrc where
  go m = \case
    B r t -> Map.insert (R.Builtin r) t m
    D r t -> Map.insert (R.Builtin r) t m
    _ -> m

typeOf :: a -> (Type -> a) -> R.Reference -> a
typeOf a f r = maybe a f (Map.lookup r termRefTypes)

builtinsSrc :: [BuiltinDSL]
builtinsSrc =
  [ B "Any.unsafeExtract" $ forall1 "a" (\a -> anyt --> a)
  , B "Int.+" $ int --> int --> int
  , B "Int.-" $ int --> int --> int
  , B "Int.*" $ int --> int --> int
  , B "Int./" $ int --> int --> int
  , B "Int.<" $ int --> int --> boolean
  , B "Int.>" $ int --> int --> boolean
  , B "Int.<=" $ int --> int --> boolean
  , B "Int.>=" $ int --> int --> boolean
  , B "Int.==" $ int --> int --> boolean
  , B "Int.and" $ int --> int --> int
  , B "Int.or" $ int --> int --> int
  , B "Int.xor" $ int --> int --> int
  , B "Int.complement" $ int --> int
  , B "Int.increment" $ int --> int
  , B "Int.isEven" $ int --> boolean
  , B "Int.isOdd" $ int --> boolean
  , B "Int.signum" $ int --> int
  , B "Int.leadingZeros" $ int --> nat
  , B "Int.negate" $ int --> int
  , B "Int.mod" $ int --> int --> int
  , B "Int.pow" $ int --> nat --> int
  , B "Int.shiftLeft" $ int --> nat --> int
  , B "Int.shiftRight" $ int --> nat --> int
  , B "Int.truncate0" $ int --> nat
  , B "Int.toText" $ int --> text
  , B "Int.fromText" $ text --> optionalt int
  , B "Int.toFloat" $ int --> float
  , B "Int.trailingZeros" $ int --> nat
  , B "Int.popCount" $ int --> nat
  , B "Int.fromRepresentation" $ nat --> int
  , B "Int.toRepresentation" $ int --> nat

  , B "Nat.*" $ nat --> nat --> nat
  , B "Nat.+" $ nat --> nat --> nat
  , B "Nat./" $ nat --> nat --> nat
  , B "Nat.<" $ nat --> nat --> boolean
  , B "Nat.<=" $ nat --> nat --> boolean
  , B "Nat.==" $ nat --> nat --> boolean
  , B "Nat.>" $ nat --> nat --> boolean
  , B "Nat.>=" $ nat --> nat --> boolean
  , B "Nat.and" $ nat --> nat --> nat
  , B "Nat.or" $ nat --> nat --> nat
  , B "Nat.xor" $ nat --> nat --> nat
  , B "Nat.complement" $ nat --> nat
  , B "Nat.drop" $ nat --> nat --> nat
  , B "Nat.fromText" $ text --> optionalt nat
  , B "Nat.increment" $ nat --> nat
  , B "Nat.isEven" $ nat --> boolean
  , B "Nat.isOdd" $ nat --> boolean
  , B "Nat.leadingZeros" $ nat --> nat
  , B "Nat.mod" $ nat --> nat --> nat
  , B "Nat.pow" $ nat --> nat --> nat
  , B "Nat.shiftLeft" $ nat --> nat --> nat
  , B "Nat.shiftRight" $ nat --> nat --> nat
  , B "Nat.sub" $ nat --> nat --> int
  , B "Nat.toFloat" $ nat --> float
  , B "Nat.toInt" $ nat --> int
  , B "Nat.toText" $ nat --> text
  , B "Nat.trailingZeros" $ nat --> nat
  , B "Nat.popCount" $ nat --> nat

  , B "Bytes.decodeNat64be" $ bytes --> optionalt (tuple [nat, bytes])
  , B "Bytes.decodeNat64le" $ bytes --> optionalt (tuple [nat, bytes])
  , B "Bytes.decodeNat32be" $ bytes --> optionalt (tuple [nat, bytes])
  , B "Bytes.decodeNat32le" $ bytes --> optionalt (tuple [nat, bytes])
  , B "Bytes.decodeNat16be" $ bytes --> optionalt (tuple [nat, bytes])
  , B "Bytes.decodeNat16le" $ bytes --> optionalt (tuple [nat, bytes])

  , B "Bytes.encodeNat64be" $ nat --> bytes
  , B "Bytes.encodeNat64le" $ nat --> bytes
  , B "Bytes.encodeNat32be" $ nat --> bytes
  , B "Bytes.encodeNat32le" $ nat --> bytes
  , B "Bytes.encodeNat16be" $ nat --> bytes
  , B "Bytes.encodeNat16le" $ nat --> bytes

  , B "Float.+" $ float --> float --> float
  , B "Float.-" $ float --> float --> float
  , B "Float.*" $ float --> float --> float
  , B "Float./" $ float --> float --> float
  , B "Float.<" $ float --> float --> boolean
  , B "Float.>" $ float --> float --> boolean
  , B "Float.<=" $ float --> float --> boolean
  , B "Float.>=" $ float --> float --> boolean
  , B "Float.==" $ float --> float --> boolean
  , B "Float.fromRepresentation" $ nat --> float
  , B "Float.toRepresentation" $ float --> nat

  -- Trigonmetric Functions
  , B "Float.acos" $ float --> float
  , B "Float.asin" $ float --> float
  , B "Float.atan" $ float --> float
  , B "Float.atan2" $ float --> float --> float
  , B "Float.cos" $ float --> float
  , B "Float.sin" $ float --> float
  , B "Float.tan" $ float --> float

  -- Hyperbolic Functions
  , B "Float.acosh" $ float --> float
  , B "Float.asinh" $ float --> float
  , B "Float.atanh" $ float --> float
  , B "Float.cosh" $ float --> float
  , B "Float.sinh" $ float --> float
  , B "Float.tanh" $ float --> float

  -- Exponential Functions
  , B "Float.exp" $ float --> float
  , B "Float.log" $ float --> float
  , B "Float.logBase" $ float --> float --> float

  -- Power Functions
  , B "Float.pow" $ float --> float --> float
  , B "Float.sqrt" $ float --> float

  -- Rounding and Remainder Functions
  , B "Float.ceiling" $ float --> int
  , B "Float.floor" $ float --> int
  , B "Float.round" $ float --> int
  , B "Float.truncate" $ float --> int

  -- Float Utils
  , B "Float.abs" $ float --> float
  , B "Float.max" $ float --> float --> float
  , B "Float.min" $ float --> float --> float
  , B "Float.toText" $ float --> text
  , B "Float.fromText" $ text --> optionalt float

  , B "Universal.==" $ forall1 "a" (\a -> a --> a --> boolean)
  -- Don't we want a Universal.!= ?

  -- Universal.compare intended as a low level function that just returns
  -- `Int` rather than some Ordering data type. If we want, later,
  -- could provide a pure Unison wrapper for Universal.compare that
  -- returns a proper data type.
  --
  -- 0 is equal, < 0 is less than, > 0 is greater than
  , B "Universal.compare" $ forall1 "a" (\a -> a --> a --> int)
  , B "Universal.>" $ forall1 "a" (\a -> a --> a --> boolean)
  , B "Universal.<" $ forall1 "a" (\a -> a --> a --> boolean)
  , B "Universal.>=" $ forall1 "a" (\a -> a --> a --> boolean)
  , B "Universal.<=" $ forall1 "a" (\a -> a --> a --> boolean)

  , B "bug" $ forall1 "a" (\a -> forall1 "b" (\b -> a --> b))
  , B "todo" $ forall1 "a" (\a -> forall1 "b" (\b -> a --> b))
  , B "Any.Any" $ forall1 "a" (\a -> a --> anyt)

  , B "Boolean.not" $ boolean --> boolean

  , B "Text.empty" text
  , B "Text.++" $ text --> text --> text
  , B "Text.take" $ nat --> text --> text
  , B "Text.drop" $ nat --> text --> text
  , B "Text.size" $ text --> nat
  , B "Text.repeat" $ nat --> text --> text
  , B "Text.==" $ text --> text --> boolean
  , D "Text.!=" $ text --> text --> boolean
  , B "Text.<=" $ text --> text --> boolean
  , B "Text.>=" $ text --> text --> boolean
  , B "Text.<" $ text --> text --> boolean
  , B "Text.>" $ text --> text --> boolean
  , B "Text.uncons" $ text --> optionalt (tuple [char, text])
  , B "Text.unsnoc" $ text --> optionalt (tuple [text, char])
  , B "Text.toCharList" $ text --> list char
  , B "Text.fromCharList" $ list char --> text
  , B "Text.toUtf8" $ text --> bytes
  , B "Text.fromUtf8.impl.v3" $ bytes --> eithert failure text
  , B "Char.toNat" $ char --> nat
  , B "Char.toText" $ char --> text
  , B "Char.fromNat" $ nat --> char

  , B "Bytes.empty" bytes
  , B "Bytes.fromList" $ list nat --> bytes
  , B "Bytes.++" $ bytes --> bytes --> bytes
  , B "Bytes.take" $ nat --> bytes --> bytes
  , B "Bytes.drop" $ nat --> bytes --> bytes
  , B "Bytes.at" $ nat --> bytes --> optionalt nat
  , B "Bytes.toList" $ bytes --> list nat
  , B "Bytes.size" $ bytes --> nat
  , B "Bytes.flatten" $ bytes --> bytes

  , B "Bytes.zlib.compress" $ bytes --> bytes
  , B "Bytes.zlib.decompress" $ bytes --> eithert text bytes
  , B "Bytes.gzip.compress" $ bytes --> bytes
  , B "Bytes.gzip.decompress" $ bytes --> eithert text bytes

   {- These are all `Bytes -> Bytes`, rather than `Bytes -> Text`.
      This is intentional: it avoids a round trip to `Text` if all
      you are doing with the bytes is dumping them to a file or a
      network socket.

      You can always `Text.fromUtf8` the results of these functions
      to get some `Text`.
    -}
  , B "Bytes.toBase16" $ bytes --> bytes
  , B "Bytes.toBase32" $ bytes --> bytes
  , B "Bytes.toBase64" $ bytes --> bytes
  , B "Bytes.toBase64UrlUnpadded" $ bytes --> bytes

  , B "Bytes.fromBase16" $ bytes --> eithert text bytes
  , B "Bytes.fromBase32" $ bytes --> eithert text bytes
  , B "Bytes.fromBase64" $ bytes --> eithert text bytes
  , B "Bytes.fromBase64UrlUnpadded" $ bytes --> eithert text bytes

  , D "List.empty" $ forall1 "a" list
  , B "List.cons" $ forall1 "a" (\a -> a --> list a --> list a)
  , Alias "List.cons" "List.+:"
  , B "List.snoc" $ forall1 "a" (\a -> list a --> a --> list a)
  , Alias "List.snoc" "List.:+"
  , B "List.take" $ forall1 "a" (\a -> nat --> list a --> list a)
  , B "List.drop" $ forall1 "a" (\a -> nat --> list a --> list a)
  , B "List.++" $ forall1 "a" (\a -> list a --> list a --> list a)
  , B "List.size" $ forall1 "a" (\a -> list a --> nat)
  , B "List.at" $ forall1 "a" (\a -> nat --> list a --> optionalt a)
  , B "Socket.toText" $ socket --> text
  , B "Handle.toText" $ handle --> text
  , B "ThreadId.toText" $ threadId --> text

  , B "Debug.watch" $ forall1 "a" (\a -> text --> a --> a)
  , B "Debug.trace" $ forall1 "a" (\a -> text --> a --> unit)
  , B "unsafe.coerceAbilities" $
      forall4 "a" "b" "e1" "e2" $ \a b e1 e2 ->
        (a --> Type.effect1 () e1 b) --> (a --> Type.effect1 () e2 b)
  , B "Scope.run" . forall2 "r" "g" $ \r g ->
      (forall1 "s" $ \s -> unit --> Type.effect () [scopet s, g] r) --> Type.effect1 () g r
  , B "Scope.ref" . forall2 "a" "s" $ \a s ->
      a --> Type.effect1 () (scopet s) (reft (Type.effects () [scopet s]) a)
  , B "Ref.read" . forall2 "a" "g" $ \a g ->
      reft g a --> Type.effect1 () g a
  , B "Ref.write" . forall2 "a" "g" $ \a g ->
      reft g a --> a --> Type.effect1 () g unit
  ] ++
  -- avoid name conflicts with Universal == < > <= >=
  [ Rename (t <> "." <> old) (t <> "." <> new)
  | t <- ["Int", "Nat", "Float", "Text"]
  , (old, new) <- [("==", "eq")
                  ,("<" , "lt")
                  ,("<=", "lteq")
                  ,(">" , "gt")
                  ,(">=", "gteq")]
  ] ++ moveUnder "io2" ioBuiltins
    ++ moveUnder "io2" mvarBuiltins
    ++ moveUnder "io2" stmBuiltins
    ++ hashBuiltins
    ++ fmap (uncurry B) codeBuiltins

moveUnder :: Text -> [(Text, Type)] -> [BuiltinDSL]
moveUnder prefix bs = bs >>= \(n,ty) -> [B n ty, Rename n (prefix <> "." <> n)]

-- builtins which have a version appended to their name (like the .v2 in IO.putBytes.v2)
-- Should be renamed to not have the version suffix
stripVersion :: [BuiltinDSL] -> [BuiltinDSL]
stripVersion bs =
  bs >>= rename where
    rename :: BuiltinDSL -> [BuiltinDSL]
    rename o@(B n _) = renameB o $ RE.matchOnceText regex n
    rename o@(Rename _ _) = [renameRename o]
    rename o = [o]

    -- When we see a B declaraiton, we add an additional Rename in the
    -- stream to rename it if it ahs a version string
    renameB :: BuiltinDSL -> Maybe (Text, RE.MatchText Text, Text) -> [BuiltinDSL]
    renameB o@(B n _) (Just (before, _, _)) = [o, Rename n before]
    renameB (Rename n _) (Just (before, _, _)) = [Rename n before]
    renameB x _ = [x]

    -- if there is already a Rename in the stream, then both sides of the
    -- rename need to have version stripped. This happens in when we move
    -- builtin IO to the io2 namespace, we might end up with:
    -- [ B IO.putBytes.v2 _, Rename IO.putBytes.v2 io2.IO.putBytes.v2]
    -- and would be become:
    -- [ B IO.putBytes.v2 _, Rename IO.putBytes.v2 IO.putBytes, Rename IO.putBytes io2.IO.putBytes ]
    renameRename :: BuiltinDSL -> BuiltinDSL
    renameRename (Rename before1 before2) = let after1 = renamed before1 (RE.matchOnceText regex before1)
                                                after2 = renamed before2 (RE.matchOnceText regex before2) in
                                                Rename after1 after2
    renameRename x = x

    renamed :: Text -> Maybe (Text, RE.MatchText Text, Text) -> Text
    renamed _ (Just (before, _, _)) = before
    renamed x _ = x

    r :: String
    r = "\\.v[0-9]+"
    regex :: RE.Regex
    regex = RE.makeRegexOpts (RE.defaultCompOpt { RE.caseSensitive = False }) RE.defaultExecOpt r

hashBuiltins :: [BuiltinDSL]
hashBuiltins =
  [ B "crypto.hash" $ forall1 "a" (\a -> hashAlgo --> a --> bytes)
  , B "crypto.hashBytes" $ hashAlgo --> bytes --> bytes
  , B "crypto.hmac" $ forall1 "a" (\a -> hashAlgo --> bytes --> a --> bytes)
  , B "crypto.hmacBytes" $ hashAlgo --> bytes --> bytes --> bytes
  ] ++
  map h [ "Sha3_512", "Sha3_256", "Sha2_512", "Sha2_256", "Blake2b_512", "Blake2b_256", "Blake2s_256" ]
  where
  hashAlgo = Type.ref() Type.hashAlgorithmRef
  h name = B ("crypto.HashAlgorithm."<>name) hashAlgo

ioBuiltins :: [(Text, Type)]
ioBuiltins =
  [ ("IO.openFile.impl.v3", text --> fmode --> iof handle)
  , ("IO.closeFile.impl.v3", handle --> iof unit)
  , ("IO.isFileEOF.impl.v3", handle --> iof boolean)
  , ("IO.isFileOpen.impl.v3", handle --> iof boolean)
  , ("IO.isSeekable.impl.v3", handle --> iof boolean)
  , ("IO.seekHandle.impl.v3", handle --> smode --> int --> iof unit)
  , ("IO.handlePosition.impl.v3", handle --> iof nat)
  , ("IO.getEnv.impl.v1", text --> iof text)
  , ("IO.getArgs.impl.v1", unit --> iof (list text))
  , ("IO.getBuffering.impl.v3", handle --> iof bmode)
  , ("IO.setBuffering.impl.v3", handle --> bmode --> iof unit)
  , ("IO.getBytes.impl.v3", handle --> nat --> iof bytes)
  , ("IO.putBytes.impl.v3", handle --> bytes --> iof unit)
  , ("IO.getLine.impl.v1", handle --> iof text)
  , ("IO.systemTime.impl.v3", unit --> iof nat)
  , ("IO.systemTimeMicroseconds.v1", unit --> io int)
  , ("IO.getTempDirectory.impl.v3", unit --> iof text)
  , ("IO.createTempDirectory.impl.v3", text --> iof text)
  , ("IO.getCurrentDirectory.impl.v3", unit --> iof text)
  , ("IO.setCurrentDirectory.impl.v3", text --> iof unit)
  , ("IO.fileExists.impl.v3", text --> iof boolean)
  , ("IO.isDirectory.impl.v3", text --> iof boolean)
  , ("IO.createDirectory.impl.v3", text --> iof unit)
  , ("IO.removeDirectory.impl.v3", text --> iof unit)
  , ("IO.renameDirectory.impl.v3", text --> text --> iof unit)
  , ("IO.directoryContents.impl.v3", text --> iof (list text))
  , ("IO.removeFile.impl.v3", text --> iof unit)
  , ("IO.renameFile.impl.v3", text --> text --> iof unit)
  , ("IO.getFileTimestamp.impl.v3", text --> iof nat)
  , ("IO.getFileSize.impl.v3", text --> iof nat)
  , ("IO.serverSocket.impl.v3", optionalt text --> text --> iof socket)
  , ("IO.listen.impl.v3", socket --> iof unit)
  , ("IO.clientSocket.impl.v3", text --> text --> iof socket)
  , ("IO.closeSocket.impl.v3", socket --> iof unit)
  , ("IO.socketPort.impl.v3", socket --> iof nat)
  , ("IO.socketAccept.impl.v3", socket --> iof socket)
  , ("IO.socketSend.impl.v3", socket --> bytes --> iof unit)
  , ("IO.socketReceive.impl.v3", socket --> nat --> iof bytes)
  , ("IO.forkComp.v2", forall1 "a" $ \a -> (unit --> io a) --> io threadId)
  , ("IO.stdHandle", stdhandle --> handle)

  , ("IO.delay.impl.v3", nat --> iof unit)
  , ("IO.kill.impl.v3", threadId --> iof unit)
  , ("IO.ref", forall1 "a" $ \a ->
        a --> io (reft (Type.effects () [Type.builtinIO ()]) a))
  , ("validateSandboxed",
        forall1 "a" $ \a -> list termLink --> a --> boolean)
  , ("Tls.newClient.impl.v3", tlsClientConfig --> socket --> iof tls)
  , ("Tls.newServer.impl.v3", tlsServerConfig --> socket --> iof tls)
  , ("Tls.handshake.impl.v3", tls --> iof unit)
  , ("Tls.send.impl.v3", tls --> bytes --> iof unit)
  , ("Tls.decodeCert.impl.v3", bytes --> eithert failure tlsSignedCert)
  , ("Tls.encodeCert", tlsSignedCert --> bytes)
  , ("Tls.decodePrivateKey", bytes --> list tlsPrivateKey)
  , ("Tls.encodePrivateKey", tlsPrivateKey --> bytes)
  , ("Tls.receive.impl.v3", tls --> iof bytes)
  , ("Tls.terminate.impl.v3", tls --> iof unit)
  , ("Tls.ClientConfig.default", text --> bytes --> tlsClientConfig)
  , ("Tls.ServerConfig.default", list tlsSignedCert --> tlsPrivateKey --> tlsServerConfig)
  , ("TLS.ClientConfig.ciphers.set", list tlsCipher --> tlsClientConfig --> tlsClientConfig)
  , ("Tls.ServerConfig.ciphers.set", list tlsCipher --> tlsServerConfig --> tlsServerConfig)
  , ("Tls.ClientConfig.certificates.set", list tlsSignedCert --> tlsClientConfig --> tlsClientConfig)
  , ("Tls.ServerConfig.certificates.set", list tlsSignedCert --> tlsServerConfig --> tlsServerConfig)
  , ("Tls.ClientConfig.versions.set", list tlsVersion --> tlsClientConfig --> tlsClientConfig)
  , ("Tls.ServerConfig.versions.set", list tlsVersion --> tlsServerConfig --> tlsServerConfig)
  ]

mvarBuiltins :: [(Text, Type)]
mvarBuiltins =
  [ ("MVar.new", forall1 "a" $ \a -> a --> io (mvar a))
  , ("MVar.newEmpty.v2", forall1 "a" $ \a -> unit --> io (mvar a))
  , ("MVar.take.impl.v3", forall1 "a" $ \a -> mvar a --> iof a)
  , ("MVar.tryTake", forall1 "a" $ \a -> mvar a --> io (optionalt a))
  , ("MVar.put.impl.v3", forall1 "a" $ \a -> mvar a --> a --> iof unit)
  , ("MVar.tryPut.impl.v3", forall1 "a" $ \a -> mvar a --> a --> iof boolean)
  , ("MVar.swap.impl.v3", forall1 "a" $ \a -> mvar a --> a --> iof a)
  , ("MVar.isEmpty", forall1 "a" $ \a -> mvar a --> io boolean)
  , ("MVar.read.impl.v3", forall1 "a" $ \a -> mvar a --> iof a)
  , ("MVar.tryRead.impl.v3", forall1 "a" $ \a -> mvar a --> iof (optionalt a))
  ]
  where
  mvar :: Type -> Type
  mvar a = Type.ref () Type.mvarRef `app` a

codeBuiltins :: [(Text, Type)]
codeBuiltins =
  [ ("Code.dependencies", code --> list termLink)
  , ("Code.isMissing", termLink --> io boolean)
  , ("Code.serialize", code --> bytes)
  , ("Code.deserialize", bytes --> eithert text code)
  , ("Code.cache_", list (tuple [termLink,code]) --> io (list termLink))
  , ("Code.validate", list (tuple [termLink,code]) --> io (optionalt failure))
  , ("Code.lookup", termLink --> io (optionalt code))
  , ("Code.display", text --> code --> text)
  , ("Value.dependencies", value --> list termLink)
  , ("Value.serialize", value --> bytes)
  , ("Value.deserialize", bytes --> eithert text value)
  , ("Value.value", forall1 "a" $ \a -> a --> value)
  , ("Value.load"
    , forall1 "a" $ \a -> value --> io (eithert (list termLink) a))
  , ("Link.Term.toText", termLink --> text)
  ]

stmBuiltins :: [(Text, Type)]
stmBuiltins =
  [ ("TVar.new", forall1 "a" $ \a -> a --> stm (tvar a))
  , ("TVar.newIO", forall1 "a" $ \a -> a --> io (tvar a))
  , ("TVar.read", forall1 "a" $ \a -> tvar a --> stm a)
  , ("TVar.readIO", forall1 "a" $ \a -> tvar a --> io a)
  , ("TVar.write", forall1 "a" $ \a -> tvar a --> a --> stm unit)
  , ("TVar.swap", forall1 "a" $ \a -> tvar a --> a --> stm a)
  , ("STM.retry", forall1 "a" $ \a -> unit --> stm a)
  , ("STM.atomically", forall1 "a" $ \a -> (unit --> stm a) --> io a)
  ]

forall1 :: Text -> (Type -> Type) -> Type
forall1 name body =
  let
    a = Var.named name
  in Type.forall () a (body $ Type.var () a)

forall2
  :: Text -> Text -> (Type -> Type -> Type) -> Type
forall2 na nb body = Type.foralls () [a,b] (body ta tb)
  where
  a = Var.named na
  b = Var.named nb
  ta = Type.var () a
  tb = Type.var () b

forall4
  :: Text -> Text -> Text -> Text
  -> (Type -> Type -> Type -> Type -> Type)
  -> Type
forall4 na nb nc nd body = Type.foralls () [a,b,c,d] (body ta tb tc td)
  where
  a = Var.named na
  b = Var.named nb
  c = Var.named nc
  d = Var.named nd
  ta = Type.var () a
  tb = Type.var () b
  tc = Type.var () c
  td = Type.var () d

app :: Type -> Type -> Type
app = Type.app ()

list :: Type -> Type
list arg = Type.list () `app` arg

optionalt :: Type -> Type
optionalt arg = DD.optionalType () `app` arg

tuple :: [Type] -> Type
tuple [t] = t
tuple ts = foldr pair (DD.unitType ()) ts

pair :: Type -> Type -> Type
pair l r = DD.pairType () `app` l `app` r

(-->) :: Type -> Type -> Type
a --> b = Type.arrow () a b
infixr -->

io, iof :: Type -> Type
io = Type.effect1 () (Type.builtinIO ())
iof = io . eithert failure

failure :: Type
failure = DD.failureType ()

eithert :: Type -> Type -> Type
eithert l r = DD.eitherType () `app` l `app` r

scopet :: Type -> Type
scopet s = Type.scopeType () `app` s

reft :: Type -> Type -> Type
reft s a = Type.refType () `app` s `app` a

socket, threadId, handle, unit :: Type
socket = Type.socket ()
threadId = Type.threadId ()
handle = Type.fileHandle ()
unit = DD.unitType ()

tls, tlsClientConfig, tlsServerConfig, tlsSignedCert, tlsPrivateKey, tlsVersion, tlsCipher :: Type
tls = Type.ref () Type.tlsRef
tlsClientConfig = Type.ref () Type.tlsClientConfigRef
tlsServerConfig = Type.ref () Type.tlsServerConfigRef
tlsSignedCert = Type.ref () Type.tlsSignedCertRef
tlsPrivateKey = Type.ref () Type.tlsPrivateKeyRef
tlsVersion = Type.ref () Type.tlsVersionRef
tlsCipher = Type.ref () Type.tlsCipherRef

fmode, bmode, smode, stdhandle :: Type
fmode = DD.fileModeType ()
bmode = DD.bufferModeType ()
smode = DD.seekModeType ()
stdhandle = DD.stdHandleType ()

int, nat, bytes, text, boolean, float, char :: Type
int = Type.int ()
nat = Type.nat ()
bytes = Type.bytes ()
text = Type.text ()
boolean = Type.boolean ()
float = Type.float ()
char = Type.char ()

anyt, code, value, termLink :: Type
anyt = Type.ref() Type.anyRef
code = Type.code ()
value = Type.value ()
termLink = Type.termLink ()

stm, tvar :: Type -> Type
stm = Type.effect1 () (Type.ref () Type.stmRef)
tvar a = Type.ref () Type.tvarRef `app` a
