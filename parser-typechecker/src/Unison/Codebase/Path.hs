{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Codebase.Path
  ( Path (..),
    Path' (..),
    Absolute (..),
    pattern AbsolutePath',
    absPath_,
    Relative (..),
    relPath_,
    pattern RelativePath',
    Resolve (..),
    pattern Empty,
    pattern (Lens.:<),
    pattern (Lens.:>),
    singleton,
    Unison.Codebase.Path.uncons,
    empty,
    isAbsolute,
    isRelative,
    absoluteEmpty,
    absoluteEmpty',
    relativeEmpty,
    relativeEmpty',
    currentPath,
    parentOfName,
    prefix,
    prefixAbs,
    prefixRel,
    maybePrefix,
    unprefix,
    maybePrefixName,
    prefixNameIfRel,
    unprefixName,
    HQSplit,
    HQSplitAbsolute,
    AbsSplit,
    Split,
    ancestors,

    -- * utilities
    longestPathPrefix,

    -- * tests
    isCurrentPath,
    isRoot,
    isRoot',

    -- * conversions
    absoluteToPath',
    fromList,
    fromName,
    fromName',
    fromPath',
    unsafeParseText,
    unsafeParseText',
    toAbsoluteSplit,
    toList,
    toName,
    toName',
    toText,
    toText',
    absToText,
    relToText,
    unsplit,
    unsplitAbsolute,
    nameFromHQSplit,
    splitFromName,

    -- * things that could be replaced with `Cons` instances
    cons,

    -- * things that could be replaced with `Snoc` instances
    snoc,
    unsnoc,
  )
where

import Control.Lens hiding (cons, snoc, unsnoc, pattern Empty)
import Control.Lens qualified as Lens
import Data.Foldable qualified as Foldable
import Data.List.Extra (dropPrefix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import GHC.Exts qualified as GHC
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.Prelude hiding (empty, toList)
import Unison.Syntax.Name qualified as Name (toText, unsafeParseText)
import Unison.Util.List qualified as List

-- | A `Path` is an internal structure representing some namespace in the codebase.
--
--  @Foo.Bar.baz@ becomes @["Foo", "Bar", "baz"]@.
--
--  __NB__:  This shouldn’t be exposed outside of this module (prefer`Path'`, `Absolute`, or `Relative`), but it’s
--   currently used pretty widely. Such usage should be replaced when encountered.
newtype Path = Path {toSeq :: Seq NameSegment}
  deriving stock (Eq, Ord)
  deriving newtype (Semigroup, Monoid)

-- | Meant for use mostly in doc-tests where it's
-- sometimes convenient to specify paths as lists.
instance GHC.IsList Path where
  type Item Path = NameSegment
  toList (Path segs) = Foldable.toList segs
  fromList = Path . Seq.fromList

-- | An absolute from the current project root
newtype Absolute = Absolute {unabsolute :: Path} deriving (Eq, Ord)

absPath_ :: Lens' Absolute Path
absPath_ = lens unabsolute (\_ new -> Absolute new)

-- | A namespace path that doesn’t necessarily start from the root.
-- Typically refers to a path from the current namespace.
newtype Relative = Relative {unrelative :: Path} deriving (Eq, Ord)

relPath_ :: Lens' Relative Path
relPath_ = lens unrelative (\_ new -> Relative new)

-- | A namespace that may be either absolute or relative, This is the most general type that should be used.
newtype Path' = Path' {unPath' :: Either Absolute Relative}
  deriving (Eq, Ord)

isAbsolute :: Path' -> Bool
isAbsolute (AbsolutePath' _) = True
isAbsolute _ = False

isRelative :: Path' -> Bool
isRelative (RelativePath' _) = True
isRelative _ = False

isCurrentPath :: Path' -> Bool
isCurrentPath p = p == currentPath

currentPath :: Path'
currentPath = Path' (Right (Relative (Path mempty)))

isRoot' :: Path' -> Bool
isRoot' = either isRoot (const False) . unPath'

isRoot :: Absolute -> Bool
isRoot = Seq.null . toSeq . unabsolute

absoluteToPath' :: Absolute -> Path'
absoluteToPath' = AbsolutePath'

instance Show Path' where
  show = \case
    AbsolutePath' abs -> Text.unpack $ absToText abs
    RelativePath' rel -> Text.unpack $ relToText rel

instance Show Absolute where
  show s = Text.unpack $ absToText s

instance Show Relative where
  show = Text.unpack . relToText

unsplit :: Split -> Path
unsplit (Path p, a) = Path (p :|> a)

unsplitAbsolute :: (Absolute, NameSegment) -> Absolute
unsplitAbsolute =
  coerce unsplit

nameFromHQSplit :: HQSplit -> HQ'.HashQualified Name
nameFromHQSplit hqs =
  let (segs, hqseg) = first (reverse . toList) hqs
   in Name.fromReverseSegments . (:| segs) <$> hqseg

type AbsSplit = (Absolute, NameSegment)

type Split = (Path, NameSegment)

type HQSplit = (Path, HQ'.HQSegment)

type HQSplitAbsolute = (Absolute, HQ'.HQSegment)

-- | examples:
--   unprefix .foo.bar .blah == .blah (absolute paths left alone)
--   unprefix .foo.bar id    == id    (relative paths starting w/ nonmatching prefix left alone)
--   unprefix .foo.bar foo.bar.baz == baz (relative paths w/ common prefix get stripped)
unprefix :: Absolute -> Path' -> Path
unprefix (Absolute prefix) = \case
  AbsolutePath' abs -> unabsolute abs
  RelativePath' rel -> fromList $ dropPrefix (toList prefix) (toList (unrelative rel))

prefixAbs :: Absolute -> Relative -> Absolute
prefixAbs prefix = Absolute . Path . (toSeq (unabsolute prefix) <>) . toSeq . unrelative

prefixRel :: Relative -> Relative -> Relative
prefixRel prefix = Relative . Path . (toSeq (unrelative prefix) <>) . toSeq . unrelative

-- | This always prefixes, since the secend argument can never be Absolute.
prefix :: Path' -> Relative -> Path'
prefix prefix =
  Path' . case prefix of
    AbsolutePath' abs -> Left . prefixAbs abs
    RelativePath' rel -> pure . prefixRel rel

-- | Returns `Nothing` if the second argument is absolute. A common pattern is
--   @fromMaybe path $ maybePrefix prefix path@ to use the unmodified path in that case.
maybePrefix :: Path' -> Path' -> Maybe Path'
maybePrefix pre = \case
  AbsolutePath' _ -> Nothing
  RelativePath' rel -> pure $ prefix pre rel

-- | Finds the longest shared path prefix of two paths.
-- Returns (shared prefix, path to first location from shared prefix, path to second location from shared prefix)
--
-- >>> longestPathPrefix ("a" :< "b" :< "x" :< Empty) ("a" :< "b" :< "c" :< Empty)
-- (a.b,x,c)
--
-- >>> longestPathPrefix Empty ("a" :< "b" :< "c" :< Empty)
-- (,,a.b.c)
longestPathPrefix :: Path -> Path -> (Path, Path, Path)
longestPathPrefix a b =
  List.splitOnLongestCommonPrefix (toList a) (toList b)
    & \(a, b, c) -> (fromList a, fromList b, fromList c)

toAbsoluteSplit :: Absolute -> Name -> (Absolute, NameSegment)
toAbsoluteSplit a = first (resolve a) . parentOfName

absoluteEmpty :: Absolute
absoluteEmpty = Absolute empty

relativeEmpty :: Relative
relativeEmpty = Relative empty

relativeEmpty' :: Path'
relativeEmpty' = RelativePath' (Relative empty)

absoluteEmpty' :: Path'
absoluteEmpty' = AbsolutePath' (Absolute empty)

-- Forget whether the path is absolute or relative
fromPath' :: Path' -> Path
fromPath' = \case
  AbsolutePath' (Absolute p) -> p
  RelativePath' (Relative p) -> p

toList :: Path -> [NameSegment]
toList = Foldable.toList . toSeq

fromList :: [NameSegment] -> Path
fromList = Path . Seq.fromList

ancestors :: Absolute -> Seq Absolute
ancestors (Absolute (Path segments)) = Absolute . Path <$> Seq.inits segments

-- |
-- >>> splitFromName "a.b.c"
-- (a.b,c)
--
-- >>> splitFromName "foo"
-- (,foo)
splitFromName :: Name -> Split
splitFromName name = case Name.reverseSegments name of
  h :| t -> (fromList $ reverse t, h)

-- | Remove a path prefix from a name.
-- Returns 'Nothing' if there are no remaining segments to construct the name from.
--
-- >>> unprefixName (Absolute $ fromList ["base", "List"]) (Name.unsafeFromText "base.List.map")
-- Just (Name Relative (NameSegment {toText = "map"} :| []))
unprefixName :: Absolute -> Name -> Maybe Name
unprefixName prefix = toName . unprefix prefix . fromName'

-- | Returns `Nothing` if the second argument is absolute. A common pattern is
--   @fromMaybe name $ maybePrefixName prefix name@ to use the unmodified path in that case.
maybePrefixName :: Path' -> Name -> Maybe Name
maybePrefixName pre name =
  if Name.isAbsolute name
    then Nothing
    else
      pure
        let newName = case Name.reverseSegments name of
              h :| t -> Name.fromReverseSegments $ h :| t <> reverse (toList $ fromPath' pre)
         in if isAbsolute pre then Name.makeAbsolute newName else newName

prefixNameIfRel :: Path' -> Name -> Name
prefixNameIfRel p name = fromMaybe name $ maybePrefixName p name

singleton :: NameSegment -> Path
singleton n = fromList [n]

cons :: NameSegment -> Path -> Path
cons = Lens.cons

snoc :: Path -> NameSegment -> Path
snoc = Lens.snoc

unsnoc :: Path -> Maybe (Path, NameSegment)
unsnoc = Lens.unsnoc

uncons :: Path -> Maybe (NameSegment, Path)
uncons = Lens.uncons

-- > Path.fromName . Name.unsafeFromText $ ".Foo.bar"
-- /Foo/bar
-- Int./  -> "Int"/"/"
-- pkg/Int.. -> "pkg"/"Int"/"."
-- Int./foo -> error because "/foo" is not a valid NameSegment
--                      and "Int." is not a valid NameSegment
--                      and "Int" / "" / "foo" is not a valid path (internal "")
-- todo: fromName needs to be a little more complicated if we want to allow
--       identifiers called Function.(.)
fromName :: Name -> Path
fromName = fromList . List.NonEmpty.toList . Name.segments

parentOfName :: Name -> (Path', NameSegment)
parentOfName name =
  let h :| t = Name.reverseSegments name
      path = fromList $ reverse t
   in ( if Name.isAbsolute name
          then AbsolutePath' (Absolute path)
          else RelativePath' (Relative path),
        h
      )

fromName' :: Name -> Path'
fromName' n
  | Name.isAbsolute n = AbsolutePath' (Absolute path)
  | otherwise = RelativePath' (Relative path)
  where
    path = fromName n

toName :: Path -> Maybe Name
toName = \case
  Path Seq.Empty -> Nothing
  (Path (p Seq.:<| ps)) ->
    Just $ Name.fromSegments (p List.NonEmpty.:| Foldable.toList ps)

-- | Convert a Path' to a Name
toName' :: Path' -> Maybe Name
toName' = \case
  AbsolutePath' p -> Name.makeAbsolute <$> toName (unabsolute p)
  RelativePath' p -> Name.makeRelative <$> toName (unrelative p)

pattern Empty :: Path
pattern Empty = Path Seq.Empty

pattern AbsolutePath' :: Absolute -> Path'
pattern AbsolutePath' p = Path' (Left p)

pattern RelativePath' :: Relative -> Path'
pattern RelativePath' p = Path' (Right p)

{-# COMPLETE AbsolutePath', RelativePath' #-}

empty :: Path
empty = Path mempty

instance Show Path where
  show = Text.unpack . toText

instance From Path Text where
  from = toText

instance From Absolute Text where
  from = absToText

instance From Relative Text where
  from = relToText

instance From Path' Text where
  from = toText'

-- | Note: This treats the path as relative.
toText :: Path -> Text
toText =
  maybe Text.empty Name.toText . toName

absToText :: Absolute -> Text
absToText abs = "." <> toText (unabsolute abs)

relToText :: Relative -> Text
relToText rel = toText (unrelative rel)

unsafeParseText :: Text -> Path
unsafeParseText = \case
  "" -> empty
  text -> fromName (Name.unsafeParseText text)

-- | Construct a Path' from a text
--
-- >>> fromText' "a.b.c"
-- a.b.c
--
-- >>> fromText' ".a.b.c"
-- .a.b.c
--
-- >>> show $ fromText' ""
-- ""
unsafeParseText' :: Text -> Path'
unsafeParseText' = \case
  "" -> RelativePath' (Relative mempty)
  "." -> AbsolutePath' (Absolute mempty)
  text -> fromName' (Name.unsafeParseText text)

toText' :: Path' -> Text
toText' path =
  case toName' path of
    Nothing -> if isAbsolute path then "." else ""
    Just name -> Name.toText name

{-# COMPLETE Empty, (:<) #-}

{-# COMPLETE Empty, (:>) #-}

deriving anyclass instance AsEmpty Path

instance Cons Path Path NameSegment NameSegment where
  _Cons = prism (uncurry cons) uncons
    where
      cons :: NameSegment -> Path -> Path
      cons ns (Path p) = Path (ns :<| p)
      uncons :: Path -> Either Path (NameSegment, Path)
      uncons p = case p of
        Path (hd :<| tl) -> Right (hd, Path tl)
        _ -> Left p

instance Cons Path' Path' NameSegment NameSegment where
  _Cons = prism (uncurry cons) uncons
    where
      cons :: NameSegment -> Path' -> Path'
      cons ns (AbsolutePath' p) = AbsolutePath' (ns :< p)
      cons ns (RelativePath' p) = RelativePath' (ns :< p)
      uncons :: Path' -> Either Path' (NameSegment, Path')
      uncons p = case p of
        AbsolutePath' (ns :< tl) -> Right (ns, AbsolutePath' tl)
        RelativePath' (ns :< tl) -> Right (ns, RelativePath' tl)
        _ -> Left p

instance Snoc Relative Relative NameSegment NameSegment where
  _Snoc = prism (uncurry snocRelative) $ \case
    Relative (Lens.unsnoc -> Just (s, a)) -> Right (Relative s, a)
    e -> Left e
    where
      snocRelative :: Relative -> NameSegment -> Relative
      snocRelative r n = Relative . (`Lens.snoc` n) $ unrelative r

instance Cons Relative Relative NameSegment NameSegment where
  _Cons = prism (uncurry cons) uncons
    where
      cons :: NameSegment -> Relative -> Relative
      cons ns (Relative p) = Relative (ns :< p)
      uncons :: Relative -> Either Relative (NameSegment, Relative)
      uncons p = case p of
        Relative (ns :< tl) -> Right (ns, Relative tl)
        _ -> Left p

instance Cons Absolute Absolute NameSegment NameSegment where
  _Cons = prism (uncurry cons) uncons
    where
      cons :: NameSegment -> Absolute -> Absolute
      cons ns (Absolute p) = Absolute (ns :< p)
      uncons :: Absolute -> Either Absolute (NameSegment, Absolute)
      uncons p = case p of
        Absolute (ns :< tl) -> Right (ns, Absolute tl)
        _ -> Left p

instance Snoc Absolute Absolute NameSegment NameSegment where
  _Snoc = prism (uncurry snocAbsolute) $ \case
    Absolute (Lens.unsnoc -> Just (s, a)) -> Right (Absolute s, a)
    e -> Left e
    where
      snocAbsolute :: Absolute -> NameSegment -> Absolute
      snocAbsolute a n = Absolute . (`Lens.snoc` n) $ unabsolute a

instance Snoc Path Path NameSegment NameSegment where
  _Snoc = prism (uncurry snoc) unsnoc
    where
      unsnoc :: Path -> Either Path (Path, NameSegment)
      unsnoc = \case
        Path (s Seq.:|> a) -> Right (Path s, a)
        e -> Left e
      snoc :: Path -> NameSegment -> Path
      snoc (Path p) ns = Path (p <> pure ns)

instance Snoc Path' Path' NameSegment NameSegment where
  _Snoc = prism (uncurry snoc') \case
    AbsolutePath' (Lens.unsnoc -> Just (s, a)) -> Right (AbsolutePath' s, a)
    RelativePath' (Lens.unsnoc -> Just (s, a)) -> Right (RelativePath' s, a)
    e -> Left e
    where
      snoc' :: Path' -> NameSegment -> Path'
      snoc' = \case
        AbsolutePath' abs -> AbsolutePath' . Absolute . Lens.snoc (unabsolute abs)
        RelativePath' rel -> RelativePath' . Relative . Lens.snoc (unrelative rel)

class Resolve l r o where
  resolve :: l -> r -> o

instance Resolve Path Path Path where
  resolve (Path l) (Path r) = Path (l <> r)

instance Resolve Relative Relative Relative where
  resolve (Relative (Path l)) (Relative (Path r)) = Relative (Path (l <> r))

instance Resolve Absolute Relative Absolute where
  resolve (Absolute l) (Relative r) = Absolute (resolve l r)

instance Resolve Absolute Relative Path' where
  resolve l r = AbsolutePath' (resolve l r)

instance Resolve Absolute Path Absolute where
  resolve (Absolute l) r = Absolute (resolve l r)

instance Resolve Path' Path' Path' where
  resolve _ a@(AbsolutePath' {}) = a
  resolve (AbsolutePath' a) (RelativePath' r) = AbsolutePath' (resolve a r)
  resolve (RelativePath' r1) (RelativePath' r2) = RelativePath' (resolve r1 r2)

instance Resolve Absolute HQSplit HQSplitAbsolute where
  resolve l (r, hq) = (resolve l (Relative r), hq)

instance Resolve Absolute Path' Absolute where
  resolve _ (AbsolutePath' a) = a
  resolve a (RelativePath' r) = resolve a r
