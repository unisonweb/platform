{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Codebase.Path
  ( Path,
    Path' (..),
    Pathy (..),
    Namey (..),
    Absolute (..),
    absPath_,
    Relative (..),
    relPath_,
    Resolve (..),
    pattern Current,
    pattern Current',
    pattern Root,
    pattern Root',
    singleton,
    isAbsolute,
    isRelative,
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
    isRoot,

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
  )
where

import Control.Lens
import Data.Foldable qualified as Foldable
import Data.List.Extra (dropPrefix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import GHC.Exts qualified as GHC
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.Prelude hiding (empty, toList)
import Unison.Syntax.Name qualified as Name (toText, unsafeParseText)
import Unison.Util.List qualified as List
import Unison.Util.Recursion (Recursive, XNor, cata, embed)

-- | A `Path` is an internal structure representing some namespace in the codebase.
--
--  @Foo.Bar.baz@ becomes @["Foo", "Bar", "baz"]@.
--
--  __NB__:  This shouldn’t be exposed outside of this module (prefer`Path'`, `Absolute`, or `Relative`), but it’s
--   currently used pretty widely. Such usage should be replaced when encountered.
newtype Path = Path {toSeq :: Seq NameSegment}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

instance Recursive Path (XNor NameSegment) where
  cata φ = cata φ . toSeq
  embed = Path . embed . fmap toSeq

-- | Meant for use mostly in doc-tests where it's
-- sometimes convenient to specify paths as lists.
instance GHC.IsList Path where
  type Item Path = NameSegment
  toList (Path segs) = Foldable.toList segs
  fromList = Path . Seq.fromList

-- | An absolute from the current project root
newtype Absolute = Absolute {unabsolute :: Path} deriving (Eq, Ord, Show)

instance Recursive Absolute (XNor NameSegment) where
  cata φ = cata φ . unabsolute
  embed = Absolute . embed . fmap unabsolute

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

pattern Current :: Relative
pattern Current = Relative (Path Seq.Empty)

pattern Current' :: Path'
pattern Current' = RelativePath' Current

isRoot :: Absolute -> Bool
isRoot = Seq.null . toSeq . unabsolute

absoluteToPath' :: Absolute -> Path'
absoluteToPath' = AbsolutePath'

type Split path = (path, NameSegment)

-- | examples:
--   unprefix foo.bar .blah == .blah (absolute paths left alone)
--   unprefix foo.bar id    == id    (relative paths starting w/ nonmatching prefix left alone)
--   unprefix foo.bar foo.bar.baz == baz (relative paths w/ common prefix get stripped)
unprefix :: Relative -> Path' -> Path'
unprefix (Relative prefix) = \case
  AbsolutePath' abs -> AbsolutePath' abs
  RelativePath' rel -> RelativePath' . Relative . fromList . dropPrefix (toList prefix) . toList $ unrelative rel

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
longestPathPrefix :: Absolute -> Absolute -> (Absolute, Relative, Relative)
longestPathPrefix a b =
  List.splitOnLongestCommonPrefix (toList $ unabsolute a) (toList $ unabsolute b)
    & \(a, b, c) -> (Absolute $ fromList a, Relative $ fromList b, Relative $ fromList c)

toAbsoluteSplit :: Absolute -> Split Path' -> Split Absolute
toAbsoluteSplit = first . resolve

pattern Root :: Absolute
pattern Root = Absolute (Path Seq.Empty)

pattern Root' :: Path'
pattern Root' = AbsolutePath' Root

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
-- >>> unprefixName (Relative $ fromList ["base", "List"]) (Name.unsafeFromText "base.List.map")
-- Just (Name Relative (NameSegment {toText = "map"} :| []))
unprefixName :: Relative -> Name -> Maybe Name
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

class Pathy path where
  ascend :: path -> Maybe path
  ascend = fmap fst . split
  descend :: path -> NameSegment -> path

  -- | This always prefixes, since the second argument can never be absolute.
  prefix :: path -> Relative -> path

  split :: path -> Maybe (Split path)

  unsplit :: Split path -> path
  unsplit = uncurry descend
  toText :: path -> Text

class (Pathy path) => Namey path where
  nameFromSplit :: Split path -> Name

  -- | Convert a path' to a `Name`
  toName :: path -> Maybe Name
  toName = fmap nameFromSplit . split

instance Pathy Path where
  descend (Path p) = Path . (p :|>)
  prefix pre = Path . (toSeq pre <>) . toSeq . unrelative
  split (Path seq) = case seq of
    Seq.Empty -> Nothing
    p :|> n -> pure (Path p, n)

  -- Note: This treats the path as relative.
  toText = maybe Text.empty Name.toText . toName

instance Namey Path where
  nameFromSplit = Name.fromReverseSegments . uncurry (flip (:|)) . first (reverse . toList)

instance Pathy Absolute where
  descend (Absolute p) = Absolute . descend p
  prefix (Absolute pre) = Absolute . prefix pre
  split (Absolute p) = first Absolute <$> split p
  toText = ("." <>) . toText . unabsolute

instance Namey Absolute where
  nameFromSplit = Name.makeAbsolute . nameFromSplit . first unabsolute

instance Pathy Relative where
  descend (Relative p) = Relative . descend p
  prefix (Relative pre) = Relative . prefix pre
  split (Relative p) = first Relative <$> split p
  toText = toText . unrelative

instance Namey Relative where
  nameFromSplit = Name.makeRelative . nameFromSplit . first unrelative

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
  toText = \case
    AbsolutePath' p -> toText p
    RelativePath' p -> toText p

instance Namey Path' where
  nameFromSplit (path, ns) = case path of
    AbsolutePath' p -> nameFromSplit (p, ns)
    RelativePath' p -> nameFromSplit (p, ns)

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
