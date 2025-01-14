module Unison.Builtin
  ( codeLookup,
    constructorType,
    names,
    builtinDataDecls,
    builtinEffectDecls,
    builtinConstructorType,
    builtinTypeDependents,
    builtinTypeDependentsOfComponent,
    builtinTypes,
    builtinTermsByType,
    builtinTermsByTypeMention,
    intrinsicTermReferences,
    intrinsicTypeReferences,
    isBuiltinType,
    typeOf,
    typeLookup,
    termRefTypes,
    termRefTypeReferences,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Regex.TDFA qualified as RE
import Unison.Builtin.Decls qualified as DD
import Unison.Builtin.Terms qualified as TD
import Unison.Codebase.CodeLookup (CodeLookup (..))
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration qualified as DD
import Unison.Hash (Hash)
import Unison.Hashing.V2.Convert qualified as H
import Unison.Name (Name)
import Unison.Names (Names (Names))
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Reference qualified as R
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name (unsafeParseText, unsafeParseVar)
import Unison.Type qualified as Type
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Util.Relation qualified as Rel
import Unison.Var qualified as Var

type DataDeclaration = DD.DataDeclaration Symbol Ann

type EffectDeclaration = DD.EffectDeclaration Symbol Ann

type Type = Type.Type Symbol ()

names :: Names
names = Names terms types
  where
    terms =
      Rel.mapRan Referent.Ref (Rel.fromMap termNameRefs)
        <> Rel.fromList
          [ (Name.unsafeParseVar vc, Referent.Con (ConstructorReference (R.DerivedId r) cid) ct)
            | (ct, (_, (r, decl))) <-
                ((CT.Data,) <$> builtinDataDecls)
                  <> ((CT.Effect,) . (second . second) DD.toDataDecl <$> builtinEffectDecls),
              ((_, vc, _), cid) <- DD.constructors' decl `zip` [0 ..]
          ]
        <> Rel.fromList
          [ (Name.unsafeParseVar v, Referent.Ref (R.DerivedId i))
            | (v, i) <- Map.toList TD.builtinTermsRef
          ]
    types =
      Rel.fromList builtinTypes
        <> Rel.fromList
          [ (Name.unsafeParseVar v, R.DerivedId r)
            | (v, (r, _)) <- builtinDataDecls
          ]
        <> Rel.fromList
          [ (Name.unsafeParseVar v, R.DerivedId r)
            | (v, (r, _)) <- builtinEffectDecls
          ]

-- note: this function is really for deciding whether `r` is a term or type,
-- but it can only answer correctly for Builtins.
isBuiltinType :: R.Reference -> Bool
isBuiltinType r = elem r . fmap snd $ builtinTypes

typeLookup :: TL.TypeLookup Symbol Ann
typeLookup =
  TL.TypeLookup
    (fmap (const Intrinsic) <$> termRefTypes)
    (Map.fromList $ map (first R.DerivedId . snd) builtinDataDecls)
    (Map.fromList $ map (first R.DerivedId . snd) builtinEffectDecls)

constructorType :: R.Reference -> Maybe CT.ConstructorType
constructorType r =
  TL.constructorType typeLookup r
    <|> Map.lookup r builtinConstructorType

builtinDataDecls :: [(Symbol, (R.Id, DataDeclaration))]
builtinDataDecls =
  [(v, (r, Intrinsic <$ d)) | (v, r, d) <- DD.builtinDataDecls]

builtinEffectDecls :: [(Symbol, (R.Id, EffectDeclaration))]
builtinEffectDecls = [(v, (r, Intrinsic <$ d)) | (v, r, d) <- DD.builtinEffectDecls]

codeLookup :: (Applicative m) => CodeLookup Symbol m Ann
codeLookup = CodeLookup (const $ pure Nothing) (const $ pure Nothing) $ \r ->
  pure $
    lookup r [(r, Right x) | (r, x) <- snd <$> builtinDataDecls]
      <|> lookup r [(r, Left x) | (r, x) <- snd <$> builtinEffectDecls]

-- Relation predicate: Domain depends on range.
builtinDependencies :: Rel.Relation R.Reference R.Reference
builtinDependencies =
  Rel.fromMultimap (Type.dependencies <$> termRefTypes)

-- a relation whose domain is types and whose range is builtin terms with that type
builtinTermsByType :: Rel.Relation R.Reference Referent.Referent
builtinTermsByType =
  Rel.fromList
    [ (H.typeToReference ty, Referent.Ref r)
      | (r, ty) <- Map.toList termRefTypes
    ]

-- a relation whose domain is types and whose range is builtin terms that mention that type
-- example: Nat.+ mentions the type `Nat`
builtinTermsByTypeMention :: Rel.Relation R.Reference Referent.Referent
builtinTermsByTypeMention =
  Rel.fromList
    [ (m, Referent.Ref r) | (r, ty) <- Map.toList termRefTypes, m <- toList $ H.typeToReferenceMentions ty
    ]

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
builtinTypes =
  Map.toList . Map.mapKeys Name.unsafeParseText $
    foldl' go mempty builtinTypesSrc
  where
    go m = \case
      B' r _ -> Map.insert r (R.Builtin r) m
      D' r -> Map.insert r (R.Builtin r) m
      Rename' r name -> case Map.lookup name m of
        Just _ ->
          error . Text.unpack $
            "tried to rename `"
              <> r
              <> "` to `"
              <> name
              <> "`, "
              <> "which already exists."
        Nothing -> case Map.lookup r m of
          Nothing ->
            error . Text.unpack $
              "tried to rename `" <> r <> "` before it was declared."
          Just t -> Map.insert name t . Map.delete r $ m
      Alias' r name -> case Map.lookup name m of
        Just _ ->
          error . Text.unpack $
            "tried to alias `"
              <> r
              <> "` to `"
              <> name
              <> "`, "
              <> "which already exists."
        Nothing -> case Map.lookup r m of
          Nothing ->
            error . Text.unpack $
              "tried to alias `" <> r <> "` before it was declared."
          Just t -> Map.insert name t m

-- WARNING: Don't delete any of these lines, only add corrections.
builtinTypesSrc :: [BuiltinTypeDSL]
builtinTypesSrc =
  [ B' "Int" CT.Data,
    B' "Nat" CT.Data,
    B' "Float" CT.Data,
    B' "Boolean" CT.Data,
    B' "Sequence" CT.Data,
    Rename' "Sequence" "List",
    B' "Text" CT.Data,
    B' "Pattern" CT.Data,
    B' "Char" CT.Data,
    B' "Effect" CT.Data,
    Rename' "Effect" "Request",
    B' "Bytes" CT.Data,
    B' "Link.Term" CT.Data,
    B' "Link.Type" CT.Data,
    B' "IO" CT.Effect,
    Rename' "IO" "io2.IO",
    B' "Handle" CT.Data,
    Rename' "Handle" "io2.Handle",
    B' "ProcessHandle" CT.Data,
    Rename' "ProcessHandle" "io2.ProcessHandle",
    B' "Socket" CT.Data,
    Rename' "Socket" "io2.Socket",
    B' "ThreadId" CT.Data,
    Rename' "ThreadId" "io2.ThreadId",
    B' "MVar" CT.Data,
    Rename' "MVar" "io2.MVar",
    B' "Code" CT.Data,
    B' "Value" CT.Data,
    B' "Any" CT.Data,
    B' "crypto.HashAlgorithm" CT.Data,
    B' "Tls" CT.Data,
    Rename' "Tls" "io2.Tls",
    B' "Tls.ClientConfig" CT.Data,
    Rename' "Tls.ClientConfig" "io2.Tls.ClientConfig",
    B' "Tls.ServerConfig" CT.Data,
    Rename' "Tls.ServerConfig" "io2.Tls.ServerConfig",
    B' "Tls.SignedCert" CT.Data,
    Rename' "Tls.SignedCert" "io2.Tls.SignedCert",
    B' "Tls.PrivateKey" CT.Data,
    Rename' "Tls.PrivateKey" "io2.Tls.PrivateKey",
    B' "Tls.Version" CT.Data,
    Rename' "Tls.Version" "io2.Tls.Version",
    B' "Tls.Cipher" CT.Data,
    Rename' "Tls.Cipher" "io2.Tls.Cipher",
    B' "TVar" CT.Data,
    Rename' "TVar" "io2.TVar",
    B' "STM" CT.Effect,
    Rename' "STM" "io2.STM",
    B' "Ref" CT.Data,
    B' "Scope" CT.Effect,
    B' "Ref.Ticket" CT.Data,
    Rename' "Ref.Ticket" "io2.Ref.Ticket",
    B' "Promise" CT.Data,
    Rename' "Promise" "io2.Promise",
    B' "TimeSpec" CT.Data,
    Rename' "TimeSpec" "io2.Clock.internals.TimeSpec",
    B' "ImmutableArray" CT.Data,
    B' "MutableArray" CT.Data,
    B' "ImmutableByteArray" CT.Data,
    B' "MutableByteArray" CT.Data,
    B' "Char.Class" CT.Data,
    B' "UDPSocket" CT.Data,
    B' "ListenSocket" CT.Data,
    B' "ClientSockAddr" CT.Data
  ]

-- rename these to "builtin" later, when builtin means intrinsic as opposed to
-- stuff that intrinsics depend on.
intrinsicTypeReferences :: Set R.Reference
intrinsicTypeReferences = foldl' go mempty builtinTypesSrc
  where
    go acc = \case
      B' r _ -> Set.insert (R.Builtin r) acc
      D' r -> Set.insert (R.Builtin r) acc
      _ -> acc

intrinsicTermReferences :: Set R.Reference
intrinsicTermReferences = Map.keysSet termRefTypes

builtinConstructorType :: Map R.Reference CT.ConstructorType
builtinConstructorType = Map.fromList [(R.Builtin r, ct) | B' r ct <- builtinTypesSrc]

data BuiltinTypeDSL = B' Text CT.ConstructorType | D' Text | Rename' Text Text | Alias' Text Text

data BuiltinDSL
  = -- simple builtin: name=ref, type
    B Text Type
  | -- deprecated builtin: name=ref, type (TBD)
    D Text Type
  | -- rename builtin: refname, newname
    -- must not appear before corresponding B/D
    -- will overwrite newname
    Rename Text Text
  | -- alias builtin: refname, newname
    -- must not appear before corresponding B/D
    -- will overwrite newname
    Alias Text Text

instance Show BuiltinDSL where
  show (B t _) = Text.unpack $ "B" <> t
  show (Rename from to) = Text.unpack $ "Rename " <> from <> " to " <> to
  show _ = ""

termNameRefs :: Map Name R.Reference
termNameRefs = Map.mapKeys Name.unsafeParseText $ foldl' go mempty (stripVersion builtinsSrc)
  where
    go m = \case
      B r _tp -> Map.insert r (R.Builtin r) m
      D r _tp -> Map.insert r (R.Builtin r) m
      Rename r name -> case Map.lookup name m of
        Just _ ->
          error . Text.unpack $
            "tried to rename `"
              <> r
              <> "` to `"
              <> name
              <> "`, "
              <> "which already exists."
        Nothing -> case Map.lookup r m of
          Nothing ->
            error . Text.unpack $
              "tried to rename `" <> r <> "` before it was declared."
          Just t -> Map.insert name t . Map.delete r $ m
      Alias r name -> case Map.lookup name m of
        Just _ ->
          error . Text.unpack $
            "tried to alias `"
              <> r
              <> "` to `"
              <> name
              <> "`, "
              <> "which already exists."
        Nothing -> case Map.lookup r m of
          Nothing ->
            error . Text.unpack $
              "tried to alias `" <> r <> "` before it was declared."
          Just t -> Map.insert name t m

termRefTypes :: Map R.TermReference Type
termRefTypes = foldl' go mempty builtinsSrc
  where
    go m = \case
      B r t -> Map.insert (R.Builtin r) t m
      D r t -> Map.insert (R.Builtin r) t m
      _ -> m

termRefTypeReferences :: Map R.TermReference R.TypeReference
termRefTypeReferences = H.typeToReference <$> termRefTypes

typeOf :: a -> (Type -> a) -> R.Reference -> a
typeOf a f r = maybe a f (Map.lookup r termRefTypes)

builtinsSrc :: [BuiltinDSL]
builtinsSrc =
  [ B "Any.unsafeExtract" $ forall1 "a" (\a -> anyt --> a),
    B "Int.+" $ int --> int --> int,
    B "Int.-" $ int --> int --> int,
    B "Int.*" $ int --> int --> int,
    B "Int./" $ int --> int --> int,
    B "Int.<" $ int --> int --> boolean,
    B "Int.>" $ int --> int --> boolean,
    B "Int.<=" $ int --> int --> boolean,
    B "Int.>=" $ int --> int --> boolean,
    B "Int.==" $ int --> int --> boolean,
    B "Int.and" $ int --> int --> int,
    B "Int.or" $ int --> int --> int,
    B "Int.xor" $ int --> int --> int,
    B "Int.complement" $ int --> int,
    B "Int.increment" $ int --> int,
    B "Int.isEven" $ int --> boolean,
    B "Int.isOdd" $ int --> boolean,
    B "Int.signum" $ int --> int,
    B "Int.leadingZeros" $ int --> nat,
    B "Int.negate" $ int --> int,
    B "Int.mod" $ int --> int --> int,
    B "Int.pow" $ int --> nat --> int,
    B "Int.shiftLeft" $ int --> nat --> int,
    B "Int.shiftRight" $ int --> nat --> int,
    B "Int.truncate0" $ int --> nat,
    B "Int.toText" $ int --> text,
    B "Int.fromText" $ text --> optionalt int,
    B "Int.toFloat" $ int --> float,
    B "Int.trailingZeros" $ int --> nat,
    B "Int.popCount" $ int --> nat,
    B "Int.fromRepresentation" $ nat --> int,
    B "Int.toRepresentation" $ int --> nat,
    B "Nat.*" $ nat --> nat --> nat,
    B "Nat.+" $ nat --> nat --> nat,
    B "Nat./" $ nat --> nat --> nat,
    B "Nat.<" $ nat --> nat --> boolean,
    B "Nat.<=" $ nat --> nat --> boolean,
    B "Nat.==" $ nat --> nat --> boolean,
    B "Nat.>" $ nat --> nat --> boolean,
    B "Nat.>=" $ nat --> nat --> boolean,
    B "Nat.and" $ nat --> nat --> nat,
    B "Nat.or" $ nat --> nat --> nat,
    B "Nat.xor" $ nat --> nat --> nat,
    B "Nat.complement" $ nat --> nat,
    B "Nat.drop" $ nat --> nat --> nat,
    B "Nat.fromText" $ text --> optionalt nat,
    B "Nat.increment" $ nat --> nat,
    B "Nat.isEven" $ nat --> boolean,
    B "Nat.isOdd" $ nat --> boolean,
    B "Nat.leadingZeros" $ nat --> nat,
    B "Nat.mod" $ nat --> nat --> nat,
    B "Nat.pow" $ nat --> nat --> nat,
    B "Nat.shiftLeft" $ nat --> nat --> nat,
    B "Nat.shiftRight" $ nat --> nat --> nat,
    B "Nat.sub" $ nat --> nat --> int,
    B "Nat.toFloat" $ nat --> float,
    B "Nat.toInt" $ nat --> int,
    B "Nat.toText" $ nat --> text,
    B "Nat.trailingZeros" $ nat --> nat,
    B "Nat.popCount" $ nat --> nat,
    B "Bytes.decodeNat64be" $ bytes --> optionalt (tuple [nat, bytes]),
    B "Bytes.decodeNat64le" $ bytes --> optionalt (tuple [nat, bytes]),
    B "Bytes.decodeNat32be" $ bytes --> optionalt (tuple [nat, bytes]),
    B "Bytes.decodeNat32le" $ bytes --> optionalt (tuple [nat, bytes]),
    B "Bytes.decodeNat16be" $ bytes --> optionalt (tuple [nat, bytes]),
    B "Bytes.decodeNat16le" $ bytes --> optionalt (tuple [nat, bytes]),
    B "Bytes.encodeNat64be" $ nat --> bytes,
    B "Bytes.encodeNat64le" $ nat --> bytes,
    B "Bytes.encodeNat32be" $ nat --> bytes,
    B "Bytes.encodeNat32le" $ nat --> bytes,
    B "Bytes.encodeNat16be" $ nat --> bytes,
    B "Bytes.encodeNat16le" $ nat --> bytes,
    B "Float.+" $ float --> float --> float,
    B "Float.-" $ float --> float --> float,
    B "Float.*" $ float --> float --> float,
    B "Float./" $ float --> float --> float,
    B "Float.<" $ float --> float --> boolean,
    B "Float.>" $ float --> float --> boolean,
    B "Float.<=" $ float --> float --> boolean,
    B "Float.>=" $ float --> float --> boolean,
    B "Float.==" $ float --> float --> boolean,
    B "Float.fromRepresentation" $ nat --> float,
    B "Float.toRepresentation" $ float --> nat,
    -- Trigonmetric Functions
    B "Float.acos" $ float --> float,
    B "Float.asin" $ float --> float,
    B "Float.atan" $ float --> float,
    B "Float.atan2" $ float --> float --> float,
    B "Float.cos" $ float --> float,
    B "Float.sin" $ float --> float,
    B "Float.tan" $ float --> float,
    -- Hyperbolic Functions
    B "Float.acosh" $ float --> float,
    B "Float.asinh" $ float --> float,
    B "Float.atanh" $ float --> float,
    B "Float.cosh" $ float --> float,
    B "Float.sinh" $ float --> float,
    B "Float.tanh" $ float --> float,
    -- Exponential Functions
    B "Float.exp" $ float --> float,
    B "Float.log" $ float --> float,
    B "Float.logBase" $ float --> float --> float,
    -- Power Functions
    B "Float.pow" $ float --> float --> float,
    B "Float.sqrt" $ float --> float,
    -- Rounding and Remainder Functions
    B "Float.ceiling" $ float --> int,
    B "Float.floor" $ float --> int,
    B "Float.round" $ float --> int,
    B "Float.truncate" $ float --> int,
    -- Float Utils
    B "Float.abs" $ float --> float,
    B "Float.max" $ float --> float --> float,
    B "Float.min" $ float --> float --> float,
    B "Float.toText" $ float --> text,
    B "Float.fromText" $ text --> optionalt float,
    B "Universal.==" $ forall1 "a" (\a -> a --> a --> boolean),
    -- Don't we want a Universal.!= ?

    -- Universal.compare intended as a low level function that just returns
    -- `Int` rather than some Ordering data type. If we want, later,
    -- could provide a pure Unison wrapper for Universal.compare that
    -- returns a proper data type.
    --
    -- 0 is equal, < 0 is less than, > 0 is greater than
    B "Universal.compare" $ forall1 "a" (\a -> a --> a --> int),
    B "Universal.>" $ forall1 "a" (\a -> a --> a --> boolean),
    B "Universal.<" $ forall1 "a" (\a -> a --> a --> boolean),
    B "Universal.>=" $ forall1 "a" (\a -> a --> a --> boolean),
    B "Universal.<=" $ forall1 "a" (\a -> a --> a --> boolean),
    B "Universal.murmurHash" $ forall1 "a" (\a -> a --> nat),
    B "bug" $ forall1 "a" (\a -> forall1 "b" (\b -> a --> b)),
    B "todo" $ forall1 "a" (\a -> forall1 "b" (\b -> a --> b)),
    B "Any.Any" $ forall1 "a" (\a -> a --> anyt),
    B "Boolean.not" $ boolean --> boolean,
    B "Text.empty" text,
    B "Text.++" $ text --> text --> text,
    B "Text.take" $ nat --> text --> text,
    B "Text.drop" $ nat --> text --> text,
    B "Text.indexOf" $ text --> text --> optionalt nat,
    B "Text.size" $ text --> nat,
    B "Text.repeat" $ nat --> text --> text,
    B "Text.==" $ text --> text --> boolean,
    D "Text.!=" $ text --> text --> boolean,
    B "Text.<=" $ text --> text --> boolean,
    B "Text.>=" $ text --> text --> boolean,
    B "Text.<" $ text --> text --> boolean,
    B "Text.>" $ text --> text --> boolean,
    B "Text.uncons" $ text --> optionalt (tuple [char, text]),
    B "Text.unsnoc" $ text --> optionalt (tuple [text, char]),
    B "Text.toCharList" $ text --> list char,
    B "Text.fromCharList" $ list char --> text,
    B "Text.reverse" $ text --> text,
    B "Text.toUppercase" $ text --> text,
    B "Text.toLowercase" $ text --> text,
    B "Text.toUtf8" $ text --> bytes,
    B "Text.fromUtf8.impl.v3" $ bytes --> eithert failure text,
    B "Text.patterns.eof" $ pat text,
    B "Text.patterns.anyChar" $ pat text,
    -- Bytes.patterns.literal : Bytes -> Pattern Bytes
    -- Bytes.patterns.word64be : Nat -> Pattern Bytes
    -- Text.patterns.literal : Text -> Pattern Text
    B "Text.patterns.literal" $ text --> pat text,
    B "Text.patterns.digit" $ pat text,
    B "Text.patterns.letter" $ pat text,
    B "Text.patterns.space" $ pat text,
    B "Text.patterns.punctuation" $ pat text,
    B "Text.patterns.charRange" $ char --> char --> pat text,
    B "Text.patterns.notCharRange" $ char --> char --> pat text,
    B "Text.patterns.charIn" $ list char --> pat text,
    B "Text.patterns.notCharIn" $ list char --> pat text,
    -- Pattern.many : Pattern a -> Pattern a
    B "Pattern.many" $ forall1 "a" (\a -> pat a --> pat a),
    B "Pattern.many.corrected" $ forall1 "a" (\a -> pat a --> pat a),
    B "Pattern.replicate" $ forall1 "a" (\a -> nat --> nat --> pat a --> pat a),
    B "Pattern.capture" $ forall1 "a" (\a -> pat a --> pat a),
    B "Pattern.captureAs" $ forall1 "a" (\a -> a --> pat a --> pat a),
    B "Pattern.join" $ forall1 "a" (\a -> list (pat a) --> pat a),
    B "Pattern.or" $ forall1 "a" (\a -> pat a --> pat a --> pat a),
    -- Pattern.run : Pattern a -> a -> Optional ([a], a)
    B "Pattern.run" $ forall1 "a" (\a -> pat a --> a --> optionalt (tuple [list a, a])),
    B "Pattern.isMatch" $ forall1 "a" (\a -> pat a --> a --> boolean),
    B "Char.toNat" $ char --> nat,
    B "Char.toText" $ char --> text,
    B "Char.fromNat" $ nat --> char,
    B "Bytes.empty" bytes,
    B "Bytes.fromList" $ list nat --> bytes,
    B "Bytes.++" $ bytes --> bytes --> bytes,
    B "Bytes.take" $ nat --> bytes --> bytes,
    B "Bytes.drop" $ nat --> bytes --> bytes,
    B "Bytes.at" $ nat --> bytes --> optionalt nat,
    B "Bytes.indexOf" $ bytes --> bytes --> optionalt nat,
    B "Bytes.toList" $ bytes --> list nat,
    B "Bytes.size" $ bytes --> nat,
    B "Bytes.flatten" $ bytes --> bytes,
    B "Bytes.zlib.compress" $ bytes --> bytes,
    B "Bytes.zlib.decompress" $ bytes --> eithert text bytes,
    B "Bytes.gzip.compress" $ bytes --> bytes,
    B "Bytes.gzip.decompress" $ bytes --> eithert text bytes,
    {- These are all `Bytes -> Bytes`, rather than `Bytes -> Text`.
       This is intentional: it avoids a round trip to `Text` if all
       you are doing with the bytes is dumping them to a file or a
       network socket.

       You can always `Text.fromUtf8` the results of these functions
       to get some `Text`.
     -}
    B "Bytes.toBase16" $ bytes --> bytes,
    B "Bytes.toBase32" $ bytes --> bytes,
    B "Bytes.toBase64" $ bytes --> bytes,
    B "Bytes.toBase64UrlUnpadded" $ bytes --> bytes,
    B "Bytes.fromBase16" $ bytes --> eithert text bytes,
    B "Bytes.fromBase32" $ bytes --> eithert text bytes,
    B "Bytes.fromBase64" $ bytes --> eithert text bytes,
    B "Bytes.fromBase64UrlUnpadded" $ bytes --> eithert text bytes,
    D "List.empty" $ forall1 "a" list,
    B "List.cons" $ forall1 "a" (\a -> a --> list a --> list a),
    Alias "List.cons" "List.+:",
    B "List.snoc" $ forall1 "a" (\a -> list a --> a --> list a),
    Alias "List.snoc" "List.:+",
    B "List.take" $ forall1 "a" (\a -> nat --> list a --> list a),
    B "List.drop" $ forall1 "a" (\a -> nat --> list a --> list a),
    B "List.++" $ forall1 "a" (\a -> list a --> list a --> list a),
    B "List.size" $ forall1 "a" (\a -> list a --> nat),
    B "List.at" $ forall1 "a" (\a -> nat --> list a --> optionalt a),
    B "Socket.toText" $ socket --> text,
    B "Handle.toText" $ handle --> text,
    B "ThreadId.toText" $ threadId --> text,
    B "Debug.watch" $ forall1 "a" (\a -> text --> a --> a),
    B "Debug.trace" $ forall1 "a" (\a -> text --> a --> unit),
    B "Debug.toText" $
      forall1 "a" (\a -> a --> optionalt (eithert text text)),
    B "unsafe.coerceAbilities" $
      forall4 "a" "b" "e1" "e2" $ \a b e1 e2 ->
        (a --> Type.effect1 () e1 b) --> (a --> Type.effect1 () e2 b),
    B "Scope.run" . forall2 "r" "g" $ \r g ->
      (forall1 "s" $ \s -> unit --> Type.effect () [scopet s, g] r) --> Type.effect1 () g r,
    B "Scope.ref" . forall2 "a" "s" $ \a s ->
      a --> Type.effect1 () (scopet s) (reft (Type.effects () [scopet s]) a),
    B "Ref.read" . forall2 "a" "g" $ \a g ->
      reft g a --> Type.effect1 () g a,
    B "Ref.write" . forall2 "a" "g" $ \a g ->
      reft g a --> a --> Type.effect1 () g unit,
    B "MutableArray.size" . forall2 "g" "a" $ \g a -> marrayt g a --> nat,
    B "MutableByteArray.size" . forall1 "g" $ \g -> mbytearrayt g --> nat,
    B "ImmutableArray.size" . forall1 "a" $ \a -> iarrayt a --> nat,
    B "ImmutableByteArray.size" $ ibytearrayt --> nat,
    B "MutableArray.copyTo!" . forall2 "g" "a" $ \g a ->
      marrayt g a
        --> nat
        --> marrayt g a
        --> nat
        --> nat
        --> Type.effect () [g, DD.exceptionType ()] unit,
    B "MutableByteArray.copyTo!" . forall1 "g" $ \g ->
      mbytearrayt g
        --> nat
        --> mbytearrayt g
        --> nat
        --> nat
        --> Type.effect () [g, DD.exceptionType ()] unit,
    B "MutableArray.read" . forall2 "g" "a" $ \g a ->
      marrayt g a --> nat --> Type.effect () [g, DD.exceptionType ()] a,
    B "MutableByteArray.read8" . forall1 "g" $ \g ->
      mbytearrayt g --> nat --> Type.effect () [g, DD.exceptionType ()] nat,
    B "MutableByteArray.read16be" . forall1 "g" $ \g ->
      mbytearrayt g --> nat --> Type.effect () [g, DD.exceptionType ()] nat,
    B "MutableByteArray.read24be" . forall1 "g" $ \g ->
      mbytearrayt g --> nat --> Type.effect () [g, DD.exceptionType ()] nat,
    B "MutableByteArray.read32be" . forall1 "g" $ \g ->
      mbytearrayt g --> nat --> Type.effect () [g, DD.exceptionType ()] nat,
    B "MutableByteArray.read40be" . forall1 "g" $ \g ->
      mbytearrayt g --> nat --> Type.effect () [g, DD.exceptionType ()] nat,
    B "MutableByteArray.read64be" . forall1 "g" $ \g ->
      mbytearrayt g --> nat --> Type.effect () [g, DD.exceptionType ()] nat,
    B "MutableArray.write" . forall2 "g" "a" $ \g a ->
      marrayt g a --> nat --> a --> Type.effect () [g, DD.exceptionType ()] unit,
    B "MutableByteArray.write8" . forall1 "g" $ \g ->
      mbytearrayt g --> nat --> nat --> Type.effect () [g, DD.exceptionType ()] unit,
    B "MutableByteArray.write16be" . forall1 "g" $ \g ->
      mbytearrayt g --> nat --> nat --> Type.effect () [g, DD.exceptionType ()] unit,
    B "MutableByteArray.write32be" . forall1 "g" $ \g ->
      mbytearrayt g --> nat --> nat --> Type.effect () [g, DD.exceptionType ()] unit,
    B "MutableByteArray.write64be" . forall1 "g" $ \g ->
      mbytearrayt g --> nat --> nat --> Type.effect () [g, DD.exceptionType ()] unit,
    B "ImmutableArray.copyTo!" . forall2 "g" "a" $ \g a ->
      marrayt g a
        --> nat
        --> iarrayt a
        --> nat
        --> nat
        --> Type.effect () [g, DD.exceptionType ()] unit,
    B "ImmutableByteArray.copyTo!" . forall1 "g" $ \g ->
      mbytearrayt g
        --> nat
        --> ibytearrayt
        --> nat
        --> nat
        --> Type.effect () [g, DD.exceptionType ()] unit,
    B "ImmutableArray.read" . forall1 "a" $ \a ->
      iarrayt a --> nat --> Type.effect1 () (DD.exceptionType ()) a,
    B "ImmutableByteArray.read8" $
      ibytearrayt --> nat --> Type.effect1 () (DD.exceptionType ()) nat,
    B "ImmutableByteArray.read16be" $
      ibytearrayt --> nat --> Type.effect1 () (DD.exceptionType ()) nat,
    B "ImmutableByteArray.read24be" $
      ibytearrayt --> nat --> Type.effect1 () (DD.exceptionType ()) nat,
    B "ImmutableByteArray.read32be" $
      ibytearrayt --> nat --> Type.effect1 () (DD.exceptionType ()) nat,
    B "ImmutableByteArray.read40be" $
      ibytearrayt --> nat --> Type.effect1 () (DD.exceptionType ()) nat,
    B "ImmutableByteArray.read64be" $
      ibytearrayt --> nat --> Type.effect1 () (DD.exceptionType ()) nat,
    B "MutableArray.freeze!" . forall2 "g" "a" $ \g a ->
      marrayt g a --> Type.effect1 () g (iarrayt a),
    B "MutableByteArray.freeze!" . forall1 "g" $ \g ->
      mbytearrayt g --> Type.effect1 () g ibytearrayt,
    B "MutableArray.freeze" . forall2 "g" "a" $ \g a ->
      marrayt g a --> nat --> nat --> Type.effect1 () g (iarrayt a),
    B "MutableByteArray.freeze" . forall1 "g" $ \g ->
      mbytearrayt g --> nat --> nat --> Type.effect1 () g ibytearrayt,
    B "Scope.array" . forall2 "s" "a" $ \s a ->
      nat --> Type.effect1 () (scopet s) (marrayt (scopet s) a),
    B "Scope.arrayOf" . forall2 "s" "a" $ \s a ->
      a --> nat --> Type.effect1 () (scopet s) (marrayt (scopet s) a),
    B "Scope.bytearray" . forall1 "s" $ \s ->
      nat --> Type.effect1 () (scopet s) (mbytearrayt (scopet s)),
    B "Scope.bytearrayOf" . forall1 "s" $ \s ->
      nat --> nat --> Type.effect1 () (scopet s) (mbytearrayt (scopet s)),
    B "Char.Class.any" charClass,
    B "Char.Class.not" $ charClass --> charClass,
    B "Char.Class.and" $ charClass --> charClass --> charClass,
    B "Char.Class.or" $ charClass --> charClass --> charClass,
    B "Char.Class.range" $ char --> char --> charClass,
    B "Char.Class.anyOf" $ list char --> charClass,
    B "Char.Class.alphanumeric" charClass,
    B "Char.Class.upper" charClass,
    B "Char.Class.lower" charClass,
    B "Char.Class.whitespace" charClass,
    B "Char.Class.control" charClass,
    B "Char.Class.printable" charClass,
    B "Char.Class.mark" charClass,
    B "Char.Class.number" charClass,
    B "Char.Class.punctuation" charClass,
    B "Char.Class.symbol" charClass,
    B "Char.Class.separator" charClass,
    B "Char.Class.letter" charClass,
    B "Char.Class.is" $
      charClass
        --> char
        --> boolean,
    B
      "Text.patterns.char"
      $ charClass --> pat text
  ]
    ++
    -- avoid name conflicts with Universal == < > <= >=
    [ Rename (t <> "." <> old) (t <> "." <> new)
      | t <- ["Int", "Nat", "Float", "Text"],
        (old, new) <-
          [ ("==", "eq"),
            ("<", "lt"),
            ("<=", "lteq"),
            (">", "gt"),
            (">=", "gteq")
          ]
    ]
    ++ moveUnder "io2" ioBuiltins
    ++ moveUnder "io2" mvarBuiltins
    ++ moveUnder "io2" stmBuiltins
    ++ moveUnder "io2" refPromiseBuiltins
    ++ hashBuiltins
    ++ cryptoBuiltins
    ++ fmap (uncurry B) codeBuiltins

moveUnder :: Text -> [(Text, Type)] -> [BuiltinDSL]
moveUnder prefix bs = bs >>= \(n, ty) -> [B n ty, Rename n (prefix <> "." <> n)]

-- builtins which have a version appended to their name (like the .v2 in IO.putBytes.v2)
-- Should be renamed to not have the version suffix
stripVersion :: [BuiltinDSL] -> [BuiltinDSL]
stripVersion bs =
  bs >>= rename
  where
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
    renameRename (Rename before1 before2) =
      let after1 = renamed before1 (RE.matchOnceText regex before1)
          after2 = renamed before2 (RE.matchOnceText regex before2)
       in Rename after1 after2
    renameRename x = x

    renamed :: Text -> Maybe (Text, RE.MatchText Text, Text) -> Text
    renamed _ (Just (before, _, _)) = before
    renamed x _ = x

    r :: String
    r = "\\.v[0-9]+"
    regex :: RE.Regex
    regex = RE.makeRegexOpts (RE.defaultCompOpt {RE.caseSensitive = False}) RE.defaultExecOpt r

hashBuiltins :: [BuiltinDSL]
hashBuiltins =
  [ B "crypto.hash" $ forall1 "a" (\a -> hashAlgo --> a --> bytes),
    B "crypto.hashBytes" $ hashAlgo --> bytes --> bytes,
    B "crypto.hmac" $ forall1 "a" (\a -> hashAlgo --> bytes --> a --> bytes),
    B "crypto.hmacBytes" $ hashAlgo --> bytes --> bytes --> bytes
  ]
    ++ map h ["Sha3_512", "Sha3_256", "Sha2_512", "Sha2_256", "Sha1", "Blake2b_512", "Blake2b_256", "Blake2s_256", "Md5"]
  where
    hashAlgo = Type.ref () Type.hashAlgorithmRef
    h name = B ("crypto.HashAlgorithm." <> name) hashAlgo

cryptoBuiltins :: [BuiltinDSL]
cryptoBuiltins =
  [ B "crypto.Ed25519.sign.impl" $
      bytes --> bytes --> bytes --> eithert failure bytes,
    B "crypto.Ed25519.verify.impl" $
      bytes --> bytes --> bytes --> eithert failure boolean,
    B "crypto.Rsa.sign.impl" $
      bytes --> bytes --> eithert failure bytes,
    B "crypto.Rsa.verify.impl" $
      bytes --> bytes --> bytes --> eithert failure boolean
  ]

ioBuiltins :: [(Text, Type)]
ioBuiltins =
  [ ("IO.openFile.impl.v3", text --> fmode --> iof handle),
    ("IO.closeFile.impl.v3", handle --> iof unit),
    ("IO.isFileEOF.impl.v3", handle --> iof boolean),
    ("IO.isFileOpen.impl.v3", handle --> iof boolean),
    ("IO.isSeekable.impl.v3", handle --> iof boolean),
    ("IO.seekHandle.impl.v3", handle --> smode --> int --> iof unit),
    ("IO.handlePosition.impl.v3", handle --> iof nat),
    ("IO.getEnv.impl.v1", text --> iof text),
    ("IO.getArgs.impl.v1", unit --> iof (list text)),
    ("IO.getBuffering.impl.v3", handle --> iof bmode),
    ("IO.setBuffering.impl.v3", handle --> bmode --> iof unit),
    ("IO.getChar.impl.v1", handle --> iof char),
    ("IO.getEcho.impl.v1", handle --> iof boolean),
    ("IO.ready.impl.v1", handle --> iof boolean),
    ("IO.setEcho.impl.v1", handle --> boolean --> iof unit),
    ("IO.getBytes.impl.v3", handle --> nat --> iof bytes),
    ("IO.getSomeBytes.impl.v1", handle --> nat --> iof bytes),
    ("IO.putBytes.impl.v3", handle --> bytes --> iof unit),
    ("IO.getLine.impl.v1", handle --> iof text),
    ("IO.systemTime.impl.v3", unit --> iof nat),
    ("IO.systemTimeMicroseconds.v1", unit --> io int),
    ("IO.getTempDirectory.impl.v3", unit --> iof text),
    ("IO.createTempDirectory.impl.v3", text --> iof text),
    ("IO.getCurrentDirectory.impl.v3", unit --> iof text),
    ("IO.setCurrentDirectory.impl.v3", text --> iof unit),
    ("IO.fileExists.impl.v3", text --> iof boolean),
    ("IO.isDirectory.impl.v3", text --> iof boolean),
    ("IO.createDirectory.impl.v3", text --> iof unit),
    ("IO.removeDirectory.impl.v3", text --> iof unit),
    ("IO.renameDirectory.impl.v3", text --> text --> iof unit),
    ("IO.directoryContents.impl.v3", text --> iof (list text)),
    ("IO.removeFile.impl.v3", text --> iof unit),
    ("IO.renameFile.impl.v3", text --> text --> iof unit),
    ("IO.getFileTimestamp.impl.v3", text --> iof nat),
    ("IO.getFileSize.impl.v3", text --> iof nat),
    ("IO.serverSocket.impl.v3", optionalt text --> text --> iof socket),
    ("IO.listen.impl.v3", socket --> iof unit),
    ("IO.clientSocket.impl.v3", text --> text --> iof socket),
    ("IO.UDP.clientSocket.impl.v1", text --> text --> iof udpSocket),
    ("IO.UDP.ClientSockAddr.toText.v1", udpClientSockAddr --> text),
    ("IO.UDP.UDPSocket.toText.impl.v1", udpSocket --> text),
    ("IO.UDP.UDPSocket.close.impl.v1", udpSocket --> iof unit),
    ("IO.UDP.serverSocket.impl.v1", text --> text --> iof udpListenSocket),
    ("IO.UDP.ListenSocket.recvFrom.impl.v1", udpListenSocket --> iof (tuple [bytes, udpClientSockAddr])),
    ("IO.UDP.ListenSocket.sendTo.impl.v1", udpListenSocket --> bytes --> udpClientSockAddr --> iof unit),
    ("IO.UDP.ListenSocket.toText.impl.v1", udpListenSocket --> text),
    ("IO.UDP.ListenSocket.close.impl.v1", udpListenSocket --> iof unit),
    ("IO.UDP.UDPSocket.recv.impl.v1", udpSocket --> iof bytes),
    ("IO.UDP.UDPSocket.send.impl.v1", udpSocket --> bytes --> iof unit),
    ("IO.closeSocket.impl.v3", socket --> iof unit),
    ("IO.socketPort.impl.v3", socket --> iof nat),
    ("IO.socketAccept.impl.v3", socket --> iof socket),
    ("IO.socketSend.impl.v3", socket --> bytes --> iof unit),
    ("IO.socketReceive.impl.v3", socket --> nat --> iof bytes),
    ("IO.forkComp.v2", forall1 "a" $ \a -> (unit --> io a) --> io threadId),
    ("IO.stdHandle", stdhandle --> handle),
    ("IO.delay.impl.v3", nat --> iof unit),
    ("IO.kill.impl.v3", threadId --> iof unit),
    ( "IO.ref",
      forall1 "a" $ \a ->
        a --> io (reft iot a)
    ),
    ("IO.process.call", text --> list text --> io nat),
    ( "IO.process.start",
      text
        --> list text
        --> io (tuple [handle, handle, handle, phandle])
    ),
    ("IO.process.kill", phandle --> io unit),
    ("IO.process.wait", phandle --> io nat),
    ("IO.process.exitCode", phandle --> io (optionalt nat)),
    ( "validateSandboxed",
      forall1 "a" $ \a -> list termLink --> a --> boolean
    ),
    ("sandboxLinks", termLink --> io (list termLink)),
    ( "Value.validateSandboxed",
      list termLink
        --> value
        --> io (eithert (list termLink) (list termLink))
    ),
    ("Tls.newClient.impl.v3", tlsClientConfig --> socket --> iof tls),
    ("Tls.newServer.impl.v3", tlsServerConfig --> socket --> iof tls),
    ("Tls.handshake.impl.v3", tls --> iof unit),
    ("Tls.send.impl.v3", tls --> bytes --> iof unit),
    ("Tls.decodeCert.impl.v3", bytes --> eithert failure tlsSignedCert),
    ("Tls.encodeCert", tlsSignedCert --> bytes),
    ("Tls.decodePrivateKey", bytes --> list tlsPrivateKey),
    ("Tls.encodePrivateKey", tlsPrivateKey --> bytes),
    ("Tls.receive.impl.v3", tls --> iof bytes),
    ("Tls.terminate.impl.v3", tls --> iof unit),
    ("Tls.ClientConfig.default", text --> bytes --> tlsClientConfig),
    ("Tls.ServerConfig.default", list tlsSignedCert --> tlsPrivateKey --> tlsServerConfig),
    ("TLS.ClientConfig.ciphers.set", list tlsCipher --> tlsClientConfig --> tlsClientConfig),
    ("Tls.ServerConfig.ciphers.set", list tlsCipher --> tlsServerConfig --> tlsServerConfig),
    ("Tls.ClientConfig.certificates.set", list tlsSignedCert --> tlsClientConfig --> tlsClientConfig),
    ("Tls.ServerConfig.certificates.set", list tlsSignedCert --> tlsServerConfig --> tlsServerConfig),
    ("Tls.ClientConfig.versions.set", list tlsVersion --> tlsClientConfig --> tlsClientConfig),
    ("Tls.ServerConfig.versions.set", list tlsVersion --> tlsServerConfig --> tlsServerConfig),
    ("Clock.internals.monotonic.v1", unit --> iof timeSpec),
    ("Clock.internals.processCPUTime.v1", unit --> iof timeSpec),
    ("Clock.internals.threadCPUTime.v1", unit --> iof timeSpec),
    ("Clock.internals.realtime.v1", unit --> iof timeSpec),
    ("Clock.internals.sec.v1", timeSpec --> int),
    ("Clock.internals.nsec.v1", timeSpec --> nat),
    ("Clock.internals.systemTimeZone.v1", int --> io (tuple [int, nat, text])),
    ( "IO.array",
      forall1 "a" $ \a ->
        nat --> io (marrayt iot a)
    ),
    ( "IO.arrayOf",
      forall1 "a" $ \a ->
        a --> nat --> io (marrayt iot a)
    ),
    ( "IO.bytearray",
      nat --> io (mbytearrayt iot)
    ),
    ( "IO.bytearrayOf",
      nat --> nat --> io (mbytearrayt iot)
    ),
    ( "IO.tryEval",
      forall1 "a" $ \a ->
        (unit --> io a) --> Type.effect () [Type.builtinIO (), DD.exceptionType ()] a
    ),
    ("IO.randomBytes", nat --> io bytes)
  ]

mvarBuiltins :: [(Text, Type)]
mvarBuiltins =
  [ ("MVar.new", forall1 "a" $ \a -> a --> io (mvar a)),
    ("MVar.newEmpty.v2", forall1 "a" $ \a -> unit --> io (mvar a)),
    ("MVar.take.impl.v3", forall1 "a" $ \a -> mvar a --> iof a),
    ("MVar.tryTake", forall1 "a" $ \a -> mvar a --> io (optionalt a)),
    ("MVar.put.impl.v3", forall1 "a" $ \a -> mvar a --> a --> iof unit),
    ("MVar.tryPut.impl.v3", forall1 "a" $ \a -> mvar a --> a --> iof boolean),
    ("MVar.swap.impl.v3", forall1 "a" $ \a -> mvar a --> a --> iof a),
    ("MVar.isEmpty", forall1 "a" $ \a -> mvar a --> io boolean),
    ("MVar.read.impl.v3", forall1 "a" $ \a -> mvar a --> iof a),
    ("MVar.tryRead.impl.v3", forall1 "a" $ \a -> mvar a --> iof (optionalt a))
  ]
  where
    mvar :: Type -> Type
    mvar a = Type.ref () Type.mvarRef `app` a

codeBuiltins :: [(Text, Type)]
codeBuiltins =
  [ ("Code.dependencies", code --> list termLink),
    ("Code.isMissing", termLink --> io boolean),
    ("Code.serialize", code --> bytes),
    ("Code.deserialize", bytes --> eithert text code),
    ("Code.cache_", list (tuple [termLink, code]) --> io (list termLink)),
    ("Code.validate", list (tuple [termLink, code]) --> io (optionalt failure)),
    ("Code.lookup", termLink --> io (optionalt code)),
    ("Code.display", text --> code --> text),
    ( "Code.validateLinks",
      list (tuple [termLink, code])
        --> Type.effect
          ()
          [DD.exceptionType ()]
          (eithert (list termLink) (list termLink))
    ),
    ("Value.dependencies", value --> list termLink),
    ("Value.serialize", value --> bytes),
    ("Value.deserialize", bytes --> eithert text value),
    ("Value.value", forall1 "a" $ \a -> a --> value),
    ( "Value.load",
      forall1 "a" $ \a -> value --> io (eithert (list termLink) a)
    ),
    ("Link.Term.toText", termLink --> text)
  ]

stmBuiltins :: [(Text, Type)]
stmBuiltins =
  [ ("TVar.new", forall1 "a" $ \a -> a --> stm (tvar a)),
    ("TVar.newIO", forall1 "a" $ \a -> a --> io (tvar a)),
    ("TVar.read", forall1 "a" $ \a -> tvar a --> stm a),
    ("TVar.readIO", forall1 "a" $ \a -> tvar a --> io a),
    ("TVar.write", forall1 "a" $ \a -> tvar a --> a --> stm unit),
    ("TVar.swap", forall1 "a" $ \a -> tvar a --> a --> stm a),
    ("STM.retry", forall1 "a" $ \a -> unit --> stm a),
    ("STM.atomically", forall1 "a" $ \a -> (unit --> stm a) --> io a)
  ]

refPromiseBuiltins :: [(Text, Type)]
refPromiseBuiltins =
  [ ("Ref.Ticket.read", forall1 "a" $ \a -> ticket a --> a),
    ("Ref.readForCas", forall1 "a" $ \a -> reft iot a --> io (ticket a)),
    ("Ref.cas", forall1 "a" $ \a -> reft iot a --> ticket a --> a --> io boolean),
    ("Promise.new", forall1 "a" $ \a -> unit --> io (promise a)),
    ("Promise.read", forall1 "a" $ \a -> promise a --> io a),
    ("Promise.tryRead", forall1 "a" $ \a -> promise a --> io (optionalt a)),
    ("Promise.write", forall1 "a" $ \a -> promise a --> a --> io boolean)
  ]
  where
    ticket :: Type -> Type
    ticket a = Type.ref () Type.ticketRef `app` a
    promise :: Type -> Type
    promise a = Type.ref () Type.promiseRef `app` a

forall1 :: Text -> (Type -> Type) -> Type
forall1 name body =
  let a = Var.named name
   in Type.forAll () a (body $ Type.var () a)

forall2 ::
  Text -> Text -> (Type -> Type -> Type) -> Type
forall2 na nb body = Type.foralls () [a, b] (body ta tb)
  where
    a = Var.named na
    b = Var.named nb
    ta = Type.var () a
    tb = Type.var () b

forall4 ::
  Text ->
  Text ->
  Text ->
  Text ->
  (Type -> Type -> Type -> Type -> Type) ->
  Type
forall4 na nb nc nd body = Type.foralls () [a, b, c, d] (body ta tb tc td)
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

infixr 9 -->

io, iof :: Type -> Type
io = Type.effect1 () (Type.builtinIO ())
iof = io . eithert failure

iot :: Type
iot = (Type.effects () [Type.builtinIO ()])

failure :: Type
failure = DD.failureType ()

eithert :: Type -> Type -> Type
eithert l r = DD.eitherType () `app` l `app` r

scopet :: Type -> Type
scopet s = Type.scopeType () `app` s

reft :: Type -> Type -> Type
reft s a = Type.refType () `app` s `app` a

ibytearrayt :: Type
ibytearrayt = Type.ibytearrayType ()

mbytearrayt :: Type -> Type
mbytearrayt g = Type.mbytearrayType () `app` g

iarrayt :: Type -> Type
iarrayt a = Type.iarrayType () `app` a

marrayt :: Type -> Type -> Type
marrayt g a = Type.marrayType () `app` g `app` a

socket, threadId, handle, phandle, unit :: Type
socket = Type.socket ()
threadId = Type.threadId ()
handle = Type.fileHandle ()
phandle = Type.processHandle ()
unit = DD.unitType ()

udpSocket, udpListenSocket, udpClientSockAddr :: Type
udpSocket = Type.udpSocket ()
udpListenSocket = Type.udpListenSocket ()
udpClientSockAddr = Type.udpClientSockAddr ()

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
anyt = Type.ref () Type.anyRef
code = Type.code ()
value = Type.value ()
termLink = Type.termLink ()

stm, tvar, pat :: Type -> Type
stm = Type.effect1 () (Type.ref () Type.stmRef)
tvar a = Type.ref () Type.tvarRef `app` a
pat a = Type.ref () Type.patternRef `app` a

charClass :: Type
charClass = Type.ref () Type.charClassRef

timeSpec :: Type
timeSpec = Type.ref () Type.timeSpecRef
