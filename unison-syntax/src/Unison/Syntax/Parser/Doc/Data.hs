{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Haskell parallel to @unison/base.Doc@.
--
--   These types have two significant parameters: @ident@ and @code@ that are expected to be parameterized by some
--   representation of identifiers and source code of the host language.
--
--   This is much more restricted than @unison/base.Doc@, but it covers everything we can parse from Haskell. The
--   mismatch with Unison is a problem, as someone can create a Unison Doc with explicit constructors or function calls,
--   have it rendered to a scratch file, and then we can’t parse it. Changing the types here to match Unison wouldn’t
--   fix the issue. We have to modify the types and parser in concert (in both Haskell and Unison) to bring them in
--   line.
module Unison.Syntax.Parser.Doc.Data where

import Data.Bifoldable (Bifoldable, bifoldr)
import Data.Bitraversable (Bitraversable, bitraverse)
import Data.Eq.Deriving (deriveEq1, deriveEq2)
import Data.Functor.Classes (Eq1 (..), Ord1 (..), Show1 (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Ord.Deriving (deriveOrd1, deriveOrd2)
import Text.Show.Deriving (deriveShow1, deriveShow2)
import Unison.Prelude hiding (Word)
import Prelude hiding (Word)

newtype UntitledSection a = UntitledSection [a]
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

newtype Paragraph a = Paragraph (NonEmpty a)
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

$(deriveEq1 ''Paragraph)
$(deriveOrd1 ''Paragraph)
$(deriveShow1 ''Paragraph)

data List a
  = BulletedList (NonEmpty (Column a))
  | NumberedList (NonEmpty (Word64, Column a))
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

instance Eq1 List where
  liftEq eqA = curry \case
    (BulletedList as, BulletedList as') -> liftEq (liftEq eqA) as as'
    (NumberedList as, NumberedList as') -> liftEq (liftEq (liftEq eqA)) as as'
    (_, _) -> False

instance Ord1 List where
  liftCompare compareA = curry \case
    (BulletedList as, BulletedList as') -> liftCompare (liftCompare compareA) as as'
    (NumberedList as, NumberedList as') -> liftCompare (liftCompare (liftCompare compareA)) as as'
    (BulletedList _, NumberedList _) -> LT
    (NumberedList _, BulletedList _) -> GT

instance Show1 List where
  liftShowsPrec showsPrecA showListA prec =
    showParen (prec <= 11) . \case
      BulletedList as ->
        showString "BulletedList "
          . liftShowsPrec (liftShowsPrec showsPrecA showListA) (liftShowList showsPrecA showListA) 11 as
      NumberedList as ->
        showString "NumberedList "
          . liftShowsPrec
            (liftShowsPrec (liftShowsPrec showsPrecA showListA) (liftShowList showsPrecA showListA))
            (liftShowList (liftShowsPrec showsPrecA showListA) (liftShowList showsPrecA showListA))
            11
            as

data Column a
  = Column (Paragraph a) (Maybe (List a))
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

instance Eq1 Column where
  liftEq eqA (Column para mlist) (Column para' mlist') =
    liftEq eqA para para' && liftEq (liftEq eqA) mlist mlist'

instance Ord1 Column where
  liftCompare compareA (Column para mlist) (Column para' mlist') =
    liftCompare compareA para para' <> liftCompare (liftCompare compareA) mlist mlist'

instance Show1 Column where
  liftShowsPrec showsPrecA showListA prec (Column para mlist) =
    showParen (prec <= 11) $
      showString "Column "
        . liftShowsPrec showsPrecA showListA 11 para
        . liftShowsPrec (liftShowsPrec showsPrecA showListA) (liftShowList showsPrecA showListA) 11 mlist

data Top code leaf a
  = Section (Paragraph leaf) [a]
  | Eval code
  | ExampleBlock code
  | CodeBlock String String
  | List' (List leaf)
  | Paragraph' (Paragraph leaf)
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

instance Bifoldable (Top code) where
  bifoldr f g z = \case
    Section para as -> foldr f (foldr g z as) para
    Eval _ -> z
    ExampleBlock _ -> z
    CodeBlock _ _ -> z
    List' list -> foldr f z list
    Paragraph' para -> foldr f z para

instance Bifunctor (Top code) where
  bimap f g = \case
    Section para as -> Section (fmap f para) $ fmap g as
    Eval code -> Eval code
    ExampleBlock code -> ExampleBlock code
    CodeBlock title body -> CodeBlock title body
    List' list -> List' $ fmap f list
    Paragraph' para -> Paragraph' $ fmap f para

instance Bitraversable (Top code) where
  bitraverse f g = \case
    Section para as -> Section <$> traverse f para <*> traverse g as
    Eval code -> pure $ Eval code
    ExampleBlock code -> pure $ ExampleBlock code
    CodeBlock title body -> pure $ CodeBlock title body
    List' list -> List' <$> traverse f list
    Paragraph' para -> Paragraph' <$> traverse f para

$(deriveEq1 ''Top)
$(deriveOrd1 ''Top)
$(deriveShow1 ''Top)
$(deriveEq2 ''Top)
$(deriveOrd2 ''Top)
$(deriveShow2 ''Top)

-- | This is a deviation from the Unison Doc data model – in Unison, Doc distinguishes between type and term links, but
--   here Doc knows nothing about what namespaces may exist.
data EmbedLink a = EmbedLink a
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

$(deriveEq1 ''EmbedLink)
$(deriveOrd1 ''EmbedLink)
$(deriveShow1 ''EmbedLink)

newtype Transclude a = Transclude a
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

$(deriveEq1 ''Transclude)
$(deriveOrd1 ''Transclude)
$(deriveShow1 ''Transclude)

newtype EmbedAnnotation ident a
  = EmbedAnnotation (Either ident a)
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

$(deriveEq1 ''EmbedAnnotation)
$(deriveOrd1 ''EmbedAnnotation)
$(deriveShow1 ''EmbedAnnotation)
$(deriveEq2 ''EmbedAnnotation)
$(deriveOrd2 ''EmbedAnnotation)
$(deriveShow2 ''EmbedAnnotation)

data SourceElement ident a = SourceElement (EmbedLink ident) [EmbedAnnotation ident a]
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

$(deriveEq1 ''SourceElement)
$(deriveOrd1 ''SourceElement)
$(deriveShow1 ''SourceElement)
$(deriveEq2 ''SourceElement)
$(deriveOrd2 ''SourceElement)
$(deriveShow2 ''SourceElement)

newtype EmbedSignatureLink a = EmbedSignatureLink a
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

newtype Word = Word String
  deriving (Eq, Ord, Show)

newtype Join a = Join (NonEmpty a)
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

$(deriveEq1 ''Join)
$(deriveOrd1 ''Join)
$(deriveShow1 ''Join)

newtype Group a = Group (Join a)
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

$(deriveEq1 ''Group)
$(deriveOrd1 ''Group)
$(deriveShow1 ''Group)

data Leaf ident code a
  = Link (EmbedLink ident)
  | -- | the Group always contains either a single Term/Type link or list of  `Transclude`s & `Word`s
    NamedLink (Paragraph a) (Group a)
  | Example code
  | Transclude' (Transclude code)
  | Bold (Paragraph a)
  | Italic (Paragraph a)
  | Strikethrough (Paragraph a)
  | Verbatim Word
  | Code Word
  | Source (NonEmpty (SourceElement ident (Transclude code)))
  | FoldedSource (NonEmpty (SourceElement ident (Transclude code)))
  | EvalInline code
  | Signature (NonEmpty (EmbedSignatureLink ident))
  | SignatureInline (EmbedSignatureLink ident)
  | Word' Word
  | Group' (Group a)
  deriving (Eq, Ord, Show, Foldable, Functor, Traversable)

instance Bifunctor (Leaf ident) where
  bimap f g = \case
    Link x -> Link x
    NamedLink para group -> NamedLink (g <$> para) $ g <$> group
    Example code -> Example $ f code
    Transclude' trans -> Transclude' $ f <$> trans
    Bold para -> Bold $ g <$> para
    Italic para -> Italic $ g <$> para
    Strikethrough para -> Strikethrough $ g <$> para
    Verbatim word -> Verbatim word
    Code word -> Code word
    Source elems -> Source $ fmap (fmap f) <$> elems
    FoldedSource elems -> FoldedSource $ fmap (fmap f) <$> elems
    EvalInline code -> EvalInline $ f code
    Signature x -> Signature x
    SignatureInline x -> SignatureInline x
    Word' word -> Word' word
    Group' group -> Group' $ g <$> group

$(deriveEq1 ''Leaf)
$(deriveOrd1 ''Leaf)
$(deriveShow1 ''Leaf)
$(deriveEq2 ''Leaf)
$(deriveOrd2 ''Leaf)
$(deriveShow2 ''Leaf)
