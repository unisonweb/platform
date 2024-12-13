{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Codebase.Path
  ( Path,
    Path' (..),
    Pathy (..),
    Absolute (..),
    absPath_,
    Relative (..),
    relPath_,
    Resolve (..),
    pattern Empty,
    pattern (Lens.:<),
    pattern (Lens.:>),
    singleton,
    Unison.Codebase.Path.uncons,
    isAbsolute,
    isRelative,
    root,
    root',
    currentPath,
    parentOfName,
    maybePrefix,
    unprefix,
    maybePrefixName,
    prefixNameIfRel,
    unprefixName,
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
    splitFromName,

    -- * things that could be replaced with `Cons` instances
    cons,
  )
where

import Control.Lens hiding (cons, pattern Empty)
import Control.Lens qualified as Lens
import Data.Foldable qualified as Foldable
import Data.List.Extra (dropPrefix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import GHC.Exts qualified as GHC
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
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

-- | Meant for use mostly in doc-tests where it's
-- sometimes convenient to specify paths as lists.
instance GHC.IsList Path where
  type Item Path = NameSegment
  toList (Path segs) = Foldable.toList segs
  fromList = Path . Seq.fromList

-- | An absolute from the current project root
newtype Absolute = Absolute {unabsolute :: Path} deriving (Eq, Ord, Show)

absPath_ :: Lens' Absolute Path
absPath_ = lens unabsolute (\_ new -> Absolute new)

-- | A namespace path that doesn’t necessarily start from the root.
-- Typically refers to a path from the current namespace.
newtype Relative = Relative {unrelative :: Path}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

relPath_ :: Lens' Relative Path
relPath_ = lens unrelative (\_ new -> Relative new)

-- | A namespace that may be either absolute or relative, This is the most general type that should be used.
data Path'
  = AbsolutePath' Absolute
  | RelativePath' Relative
  deriving (Eq, Ord, Show)

isAbsolute :: Path' -> Bool
isAbsolute (AbsolutePath' _) = True
isAbsolute _ = False

isRelative :: Path' -> Bool
isRelative (RelativePath' _) = True
isRelative _ = False

isCurrentPath :: Path' -> Bool
isCurrentPath p = p == currentPath

currentPath :: Path'
currentPath = RelativePath' mempty

isRoot' :: Path' -> Bool
isRoot' = \case
  AbsolutePath' p -> isRoot p
  RelativePath' _ -> False

isRoot :: Absolute -> Bool
isRoot = Seq.null . toSeq . unabsolute

absoluteToPath' :: Absolute -> Path'
absoluteToPath' = AbsolutePath'

type Split path = (path, NameSegment)

-- | examples:
--   unprefix .foo.bar .blah == .blah (absolute paths left alone)
--   unprefix .foo.bar id    == id    (relative paths starting w/ nonmatching prefix left alone)
--   unprefix .foo.bar foo.bar.baz == baz (relative paths w/ common prefix get stripped)
unprefix :: Absolute -> Path' -> Path
unprefix (Absolute prefix) = \case
  AbsolutePath' abs -> unabsolute abs
  RelativePath' rel -> fromList $ dropPrefix (toList prefix) (toList (unrelative rel))

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

toAbsoluteSplit :: Absolute -> Name -> Split Absolute
toAbsoluteSplit a = first (resolve a) . parentOfName

root :: Absolute
root = Absolute mempty

root' :: Path'
root' = AbsolutePath' root

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
splitFromName :: Name -> Split Path
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

class Pathy path where
  ascend :: path -> Maybe path
  ascend = fmap fst . split
  descend :: path -> NameSegment -> path

  -- | This always prefixes, since the second argument can never be absolute.
  prefix :: path -> Relative -> path

  split :: path -> Maybe (Split path)
  nameFromSplit :: Split path -> Name

  -- | Convert a path' to a `Name`
  toName :: path -> Maybe Name
  toName = fmap nameFromSplit . split

  unsplit :: Split path -> path
  toText :: path -> Text

instance Pathy Path where
  descend (Path p) ns = Path (p <> pure ns)
  prefix pre = Path . (toSeq pre <>) . toSeq . unrelative
  split (Path seq) = case seq of
    Seq.Empty -> Nothing
    p :|> n -> pure (Path p, n)
  nameFromSplit = Name.fromReverseSegments . uncurry (flip (:|)) . first (reverse . toList)
  unsplit (Path p, a) = Path (p :|> a)

  -- Note: This treats the path as relative.
  toText = maybe Text.empty Name.toText . toName

instance Pathy Absolute where
  descend (Absolute p) = Absolute . descend p
  prefix (Absolute pre) = Absolute . prefix pre
  split (Absolute p) = first Absolute <$> split p
  nameFromSplit = Name.makeAbsolute . nameFromSplit . first unabsolute
  unsplit = Absolute . unsplit . first unabsolute
  toText = ("." <>) . toText . unabsolute

instance Pathy Relative where
  descend (Relative p) = Relative . descend p
  prefix (Relative pre) = Relative . prefix pre
  split (Relative p) = first Relative <$> split p
  nameFromSplit = Name.makeRelative . nameFromSplit . first unrelative
  unsplit = Relative . unsplit . first unrelative
  toText = toText . unrelative

instance Pathy Path' where
  descend = \case
    AbsolutePath' p -> AbsolutePath' . descend p
    RelativePath' p -> RelativePath' . descend p
  prefix = \case
    AbsolutePath' p -> AbsolutePath' . prefix p
    RelativePath' p -> RelativePath' . prefix p
  split = \case
    AbsolutePath' p -> first AbsolutePath' <$> split p
    RelativePath' p -> first RelativePath' <$> split p
  nameFromSplit (path, ns) = case path of
    AbsolutePath' p -> nameFromSplit (p, ns)
    RelativePath' p -> nameFromSplit (p, ns)
  unsplit (path, ns) = case path of
    AbsolutePath' p -> AbsolutePath' $ unsplit (p, ns)
    RelativePath' p -> RelativePath' $ unsplit (p, ns)
  toText = \case
    AbsolutePath' p -> toText p
    RelativePath' p -> toText p

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

parentOfName :: Name -> Split Path'
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

pattern Empty :: Path
pattern Empty = Path Seq.Empty

-- instance From Path Text where
--   from = toText

-- instance From Absolute Text where
--   from = toText

-- instance From Relative Text where
--   from = toText

-- instance From Path' Text where
--   from = toText

unsafeParseText :: Text -> Path
unsafeParseText = \case
  "" -> mempty
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

instance Resolve Absolute (Split Path) (Split Absolute) where
  resolve l r = first (resolve l) r

instance Resolve Absolute Path' Absolute where
  resolve _ (AbsolutePath' a) = a
  resolve a (RelativePath' r) = resolve a r
