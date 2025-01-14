{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.Decompile
  ( decompile,
    DecompResult,
    DecompError (..),
    renderDecompError,
  )
where

import Data.Set (singleton)
import Unison.ABT (substs)
import Unison.Codebase.Runtime (Error)
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Prelude
import Unison.Reference (Reference, pattern Builtin)
import Unison.Referent (pattern Ref)
import Unison.Runtime.ANF (maskTags)
import Unison.Runtime.Array
  ( Array,
    ByteArray,
    byteArrayToList,
  )
import Unison.Runtime.Foreign
  ( Foreign (..),
    HashAlgorithm (..),
    maybeUnwrapBuiltin,
    maybeUnwrapForeign,
  )
import Unison.Runtime.IOSource (iarrayFromListRef, ibarrayFromBytesRef)
import Unison.Runtime.MCode (CombIx (..))
import Unison.Runtime.Stack
  ( Closure (..),
    USeq,
    UnboxedTypeTag (..),
    Val (..),
    pattern DataC,
    pattern PApV,
  )
import Unison.Syntax.NamePrinter (prettyReference)
import Unison.Term
  ( Term,
    app,
    apps',
    boolean,
    builtin,
    char,
    constructor,
    float,
    int,
    list,
    list',
    nat,
    ref,
    termLink,
    text,
    typeLink,
    pattern LamNamed',
  )
import Unison.Term qualified as Term
import Unison.Type
  ( anyRef,
    booleanRef,
    iarrayRef,
    ibytearrayRef,
    listRef,
    termLinkRef,
    typeLinkRef,
  )
import Unison.Util.Bytes qualified as By
import Unison.Util.Pretty (indentN, lines, lit, shown, syntaxToColor, wrap)
import Unison.Util.Text qualified as Text
import Unison.Var (Var)
import Prelude hiding (lines)

con :: (Var v) => Reference -> Word64 -> Term v ()
con rf ct = constructor () (ConstructorReference rf $ fromIntegral ct)

bug :: (Var v) => Text -> Term v ()
bug msg = app () (builtin () "bug") (text () msg)

err :: DecompError -> a -> (Set DecompError, a)
err err x = (singleton err, x)

data DecompError
  = BadBool !Word64
  | BadUnboxed !UnboxedTypeTag
  | BadForeign !Reference
  | BadData !Reference
  | BadPAp !Reference
  | UnkComb !Reference
  | UnkLocal !Reference !Word64
  | Cont
  | Exn
  deriving (Eq, Ord)

type DecompResult v = (Set DecompError, Term v ())

prf :: Reference -> Error
prf = syntaxToColor . prettyReference 10

printUnboxedTypeTag :: UnboxedTypeTag -> Error
printUnboxedTypeTag = shown

renderDecompError :: DecompError -> Error
renderDecompError (BadBool n) =
  lines
    [ wrap "A boolean value had an unexpected constructor tag:",
      indentN 2 . lit . fromString $ show n
    ]
renderDecompError (BadUnboxed tt) =
  lines
    [ wrap "An apparent numeric type had an unrecognized packed tag:",
      indentN 2 $ printUnboxedTypeTag tt
    ]
renderDecompError (BadForeign rf) =
  lines
    [ wrap "A foreign value with no decompiled representation was encountered:",
      indentN 2 $ prf rf
    ]
renderDecompError (BadData rf) =
  lines
    [ wrap
        "A data type with no decompiled representation was encountered:",
      indentN 2 $ prf rf
    ]
renderDecompError (BadPAp rf) =
  lines
    [ wrap "A partial function application could not be decompiled: ",
      indentN 2 $ prf rf
    ]
renderDecompError (UnkComb rf) =
  lines
    [ wrap "A reference to an unknown function was encountered: ",
      indentN 2 $ prf rf
    ]
renderDecompError (UnkLocal rf n) =
  lines
    [ "A reference to an unknown portion to a function was encountered: ",
      indentN 2 $ "function: " <> prf rf,
      indentN 2 $ "section: " <> lit (fromString $ show n)
    ]
renderDecompError Cont = "A continuation value was encountered"
renderDecompError Exn = "An exception value was encountered"

decompile ::
  forall v.
  (Var v) =>
  (Reference -> Maybe Reference) ->
  (Word64 -> Word64 -> Maybe (Term v ())) ->
  Val ->
  DecompResult v
decompile backref topTerms = \case
  CharVal c -> pure (char () c)
  NatVal n -> pure (nat () n)
  IntVal i -> pure (int () (fromIntegral i))
  DoubleVal f -> pure (float () f)
  Val i (UnboxedTypeTag tt) ->
    err (BadUnboxed tt) . nat () $ fromIntegral $ i
  Val _u clos -> case clos of
    DataC rf (maskTags -> ct) []
      | rf == booleanRef -> tag2bool ct
    (DataC rf _ [b])
      | rf == anyRef ->
          app () (builtin () "Any.Any") <$> decompile backref topTerms b
    (DataC rf (maskTags -> ct) vs) ->
      apps' (con rf ct) <$> traverse (decompile backref topTerms) vs
    (PApV (CIx rf rt k) _ vs)
      | rf == Builtin "jumpCont" ->
          err Cont $ bug "<Continuation>"
      | Builtin nm <- rf ->
          apps' (builtin () nm) <$> traverse (decompile backref topTerms) vs
      | Just t <- topTerms rt k ->
          Term.etaReduceEtaVars . substitute t
            <$> traverse (decompile backref topTerms) vs
      | k > 0,
        Just _ <- topTerms rt 0 ->
          err (UnkLocal rf k) $ bug "<Unknown>"
      | otherwise -> err (UnkComb rf) $ ref () rf
    (PAp (CIx rf _ _) _ _) ->
      err (BadPAp rf) $ bug "<Unknown>"
    BlackHole -> err Exn $ bug "<Exception>"
    (Captured {}) -> err Cont $ bug "<Continuation>"
    (Foreign f) ->
      decompileForeign backref topTerms f

tag2bool :: (Var v) => Word64 -> DecompResult v
tag2bool 0 = pure (boolean () False)
tag2bool 1 = pure (boolean () True)
tag2bool n = err (BadBool n) $ con booleanRef n

substitute :: (Var v) => Term v () -> [Term v ()] -> Term v ()
substitute = align []
  where
    align vts (LamNamed' v bd) (t : ts) = align ((v, t) : vts) bd ts
    align vts tm [] = substs vts tm
    -- this should not happen
    align vts tm ts = apps' (substs vts tm) ts

decompileForeign ::
  (Var v) =>
  (Reference -> Maybe Reference) ->
  (Word64 -> Word64 -> Maybe (Term v ())) ->
  Foreign ->
  DecompResult v
decompileForeign backref topTerms f
  | Just t <- maybeUnwrapBuiltin f = pure $ text () (Text.toText t)
  | Just b <- maybeUnwrapBuiltin f = pure $ decompileBytes b
  | Just h <- maybeUnwrapBuiltin f = pure $ decompileHashAlgorithm h
  | Just l <- maybeUnwrapForeign termLinkRef f =
      pure . termLink () $ case l of
        Ref r -> maybe l Ref $ backref r
        _ -> l
  | Just l <- maybeUnwrapForeign typeLinkRef f =
      pure $ typeLink () l
  | Just (a :: Array Val) <- maybeUnwrapForeign iarrayRef f =
      app () (ref () iarrayFromListRef) . list ()
        <$> traverse (decompile backref topTerms) (toList a)
  | Just (a :: ByteArray) <- maybeUnwrapForeign ibytearrayRef f =
      pure $
        app
          ()
          (ref () ibarrayFromBytesRef)
          (decompileBytes . By.fromWord8s $ byteArrayToList a)
  | Just s <- unwrapSeq f =
      list' () <$> traverse (decompile backref topTerms) s
decompileForeign _ _ (Wrap r _) =
  err (BadForeign r) $ bug text
  where
    text
      | Builtin name <- r = "<" <> name <> ">"
      | otherwise = "<Foreign>"

decompileBytes :: (Var v) => By.Bytes -> Term v ()
decompileBytes =
  app () (builtin () $ fromString "Bytes.fromList")
    . list ()
    . fmap (nat () . fromIntegral)
    . By.toWord8s

decompileHashAlgorithm :: (Var v) => HashAlgorithm -> Term v ()
decompileHashAlgorithm (HashAlgorithm r _) = ref () r

unwrapSeq :: Foreign -> Maybe USeq
unwrapSeq = maybeUnwrapForeign listRef
