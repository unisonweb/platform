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
    charRef,
    floatRef,
    iarrayRef,
    ibytearrayRef,
    intRef,
    listRef,
    natRef,
    termLinkRef,
    typeLinkRef,
  )
import Unison.Util.Bytes qualified as By
import Unison.Util.Pretty (indentN, lines, lit, syntaxToColor, wrap)
import Unison.Util.Text qualified as Text
import Unison.Var (Var)
import Unsafe.Coerce -- for Int -> Double
import Prelude hiding (lines)

con :: (Var v) => Reference -> Word64 -> Term v ()
con rf ct = constructor () (ConstructorReference rf $ fromIntegral ct)

bug :: (Var v) => Text -> Term v ()
bug msg = app () (builtin () "bug") (text () msg)

err :: DecompError -> a -> (Set DecompError, a)
err err x = (singleton err, x)

data DecompError
  = BadBool !Word64
  | BadUnboxed !Reference
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

renderDecompError :: DecompError -> Error
renderDecompError (BadBool n) =
  lines
    [ wrap "A boolean value had an unexpected constructor tag:",
      indentN 2 . lit . fromString $ show n
    ]
renderDecompError (BadUnboxed rf) =
  lines
    [ wrap "An apparent numeric type had an unrecognized reference:",
      indentN 2 $ prf rf
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
  (Var v) =>
  (Reference -> Maybe Reference) ->
  (Word64 -> Word64 -> Maybe (Term v ())) ->
  Closure ->
  DecompResult v
decompile backref topTerms = \case
  DataC rf (maskTags -> ct) []
    | rf == booleanRef -> tag2bool ct
  DataC rf (maskTags -> ct) [Left i] ->
    decompileUnboxed rf ct i
  (DataC rf _ [Right b])
    | rf == anyRef ->
        app () (builtin () "Any.Any") <$> decompile backref topTerms b
  (DataC rf (maskTags -> ct) vs)
    -- Only match lists of boxed args.
    | ([], bs) <- partitionEithers vs ->
        apps' (con rf ct) <$> traverse (decompile backref topTerms) bs
  (PApV (CIx rf rt k) _ (partitionEithers -> ([], bs)))
    | rf == Builtin "jumpCont" ->
        err Cont $ bug "<Continuation>"
    | Builtin nm <- rf ->
        apps' (builtin () nm) <$> traverse (decompile backref topTerms) bs
    | Just t <- topTerms rt k ->
        Term.etaReduceEtaVars . substitute t
          <$> traverse (decompile backref topTerms) bs
    | k > 0,
      Just _ <- topTerms rt 0 ->
        err (UnkLocal rf k) $ bug "<Unknown>"
    | otherwise -> err (UnkComb rf) $ ref () rf
  (PAp (CIx rf _ _) _ _) ->
    err (BadPAp rf) $ bug "<Unknown>"
  (DataC rf _ _) -> err (BadData rf) $ bug "<Data>"
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

decompileUnboxed ::
  (Var v) => Reference -> Word64 -> Int -> DecompResult v
decompileUnboxed r _ i
  | r == natRef = pure . nat () $ fromIntegral i
  | r == intRef = pure . int () $ fromIntegral i
  | r == floatRef = pure . float () $ unsafeCoerce i
  | r == charRef = pure . char () $ toEnum i
  | otherwise = err (BadUnboxed r) . nat () $ fromIntegral i

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
  | Just (a :: Array Closure) <- maybeUnwrapForeign iarrayRef f =
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

unwrapSeq :: Foreign -> Maybe (Seq Closure)
unwrapSeq = maybeUnwrapForeign listRef
