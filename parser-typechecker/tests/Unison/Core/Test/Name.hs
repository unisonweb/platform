module Unison.Core.Test.Name where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Set qualified as Set
import EasyTest
import Unison.Name as Name
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Syntax.Name qualified as Name (unsafeParseText)
import Unison.Util.Relation qualified as R

test :: Test ()
test =
  scope "name" $
    tests
      [ scope "compareSuffix" (tests testCompareSuffix),
        scope "endsWithReverseSegments" (tests testEndsWithReverseSegments),
        scope "endsWithSegments" (tests testEndsWithSegments),
        scope "segments" (tests testSegments),
        scope "splitName" (tests testSplitName),
        scope "suffixSearch" (tests testSuffixSearch),
        scope "suffixes" (tests testSuffixes),
        scope "unsafeFromString" (tests testUnsafeFromString)
      ]

testCompareSuffix :: [Test ()]
testCompareSuffix =
  [ scope "[b.c a.b.c]" (expectEqual (compareSuffix (Name.unsafeParseText "b.c") (Name.unsafeParseText "a.b.c")) EQ),
    scope "[a.b.c a.b.c]" (expectEqual (compareSuffix (Name.unsafeParseText "a.b.c") (Name.unsafeParseText "a.b.c")) EQ),
    scope "[b.c a.b.b]" (expectEqual (compareSuffix (Name.unsafeParseText "b.c") (Name.unsafeParseText "a.b.b")) LT),
    scope "[a.b.c b.c]" (expectEqual (compareSuffix (Name.unsafeParseText "a.b.c") (Name.unsafeParseText "b.c")) LT),
    scope "[b.b a.b.c]" (expectEqual (compareSuffix (Name.unsafeParseText "b.b") (Name.unsafeParseText "a.b.c")) GT)
  ]

testEndsWithReverseSegments :: [Test ()]
testEndsWithReverseSegments =
  [ scope "a.b.c ends with []" (expectEqual True (endsWithReverseSegments (Name.unsafeParseText "a.b.c") [])),
    scope
      "a.b.c ends with [c, b]"
      (expectEqual True (endsWithReverseSegments (Name.unsafeParseText "a.b.c") [NameSegment "c", NameSegment "b"])),
    scope
      "a.b.c doesn't end with [d]"
      (expectEqual False (endsWithReverseSegments (Name.unsafeParseText "a.b.c") [NameSegment "d"]))
  ]

testEndsWithSegments :: [Test ()]
testEndsWithSegments =
  [ scope "a.b.c ends with []" (expectEqual True (endsWithSegments (Name.unsafeParseText "a.b.c") [])),
    scope
      "a.b.c ends with [b, c]"
      (expectEqual True (endsWithSegments (Name.unsafeParseText "a.b.c") [NameSegment "b", NameSegment "c"])),
    scope
      "a.b.c doesn't end with [d]"
      (expectEqual False (endsWithSegments (Name.unsafeParseText "a.b.c") [NameSegment "d"]))
  ]

testSegments :: [Test ()]
testSegments =
  [ do
      n <- int' 1 10
      segs <- List.NonEmpty.fromList <$> listOf n (pick [NameSegment ".", NameSegment "foo"])
      expectEqual (segments (fromSegments segs)) segs
  ]

testSplitName :: [Test ()]
testSplitName =
  [ scope "x" (expectEqual (splits (Name.unsafeParseText "x")) [([], Name.unsafeParseText "x")]),
    scope "A.x" (expectEqual (splits (Name.unsafeParseText "A.x")) [([], Name.unsafeParseText "A.x"), ([NameSegment "A"], Name.unsafeParseText "x")]),
    scope
      "A.B.x"
      ( expectEqual
          (splits (Name.unsafeParseText "A.B.x"))
          [ ([], Name.unsafeParseText "A.B.x"),
            ([NameSegment "A"], Name.unsafeParseText "B.x"),
            ([NameSegment "A", NameSegment "B"], Name.unsafeParseText "x")
          ]
      )
  ]

testSuffixes :: [Test ()]
testSuffixes =
  [ scope "one namespace" $ expectEqual (suffixes (Name.unsafeParseText "bar")) [Name.unsafeParseText "bar"],
    scope "two namespaces" $ expectEqual (suffixes (Name.unsafeParseText "foo.bar")) [Name.unsafeParseText "bar", Name.unsafeParseText "foo.bar"],
    scope "multiple namespaces" $ expectEqual (suffixes (Name.unsafeParseText "foo.bar.baz")) [Name.unsafeParseText "baz", Name.unsafeParseText "bar.baz", Name.unsafeParseText "foo.bar.baz"],
    scope "terms named `.`" $ expectEqual (suffixes (Name.unsafeParseText "base.`.`")) [Name.unsafeParseText "`.`", Name.unsafeParseText "base.`.`"]
  ]

testSuffixSearch :: [Test ()]
testSuffixSearch =
  [ do
      let rel :: R.Relation Name Int
          rel =
            R.fromList
              [ (n "base.List.map", 1),
                (n "base.Set.map", 2),
                (n "foo.bar.baz", 3),
                (n "a.b.c", 4),
                (n "a1.b.c", 5),
                (n ".`.`", 6)
              ]
          n = Name.unsafeParseText
      expectEqual' (NameSegment "." :| []) (Name.reverseSegments (n ".`.`"))
      expectEqual' (NameSegment "." :| []) (Name.reverseSegments (n ".`.`"))

      expectEqual' (Set.fromList [1, 2]) (Name.searchBySuffix (n "map") rel)
      expectEqual' (n "List.map") (Name.suffixifyByHash (n "base.List.map") rel)
      expectEqual' (n "Set.map") (Name.suffixifyByHash (n "base.Set.map") rel)
      expectEqual' (n "baz") (Name.suffixifyByHash (n "foo.bar.baz") rel)
      expectEqual' (n "a.b.c") (Name.suffixifyByHash (n "a.b.c") rel)
      expectEqual' (n "a1.b.c") (Name.suffixifyByHash (n "a1.b.c") rel)
      note . show $ Name.reverseSegments (n "`.`")
      note . show $ Name.reverseSegments (n ".`.`")
      tests
        [ scope "(.) shortest unique suffix" $ expectEqual' (n "`.`") (Name.suffixifyByHash (n ".`.`") rel),
          scope "(.) search by suffix" $ expectEqual' (Set.fromList [6]) (Name.searchBySuffix (n "`.`") rel)
        ]
      ok
  ]

testUnsafeFromString :: [Test ()]
testUnsafeFromString =
  [ scope "." do
      expectEqual' (isAbsolute (Name.unsafeParseText "`.`")) False
      expectEqual' (segments (Name.unsafeParseText "`.`")) (NameSegment "." :| [])
      ok,
    scope ".`.`" do
      expectEqual' (isAbsolute (Name.unsafeParseText ".`.`")) True
      expectEqual' (segments (Name.unsafeParseText ".`.`")) (NameSegment "." :| [])
      ok,
    scope "foo.bar" do
      expectEqual' (isAbsolute (Name.unsafeParseText "foo.bar")) False
      expectEqual' (segments (Name.unsafeParseText "foo.bar")) (NameSegment "foo" :| [NameSegment "bar"])
      ok,
    scope ".foo.bar" do
      expectEqual' (isAbsolute (Name.unsafeParseText ".foo.bar")) True
      expectEqual' (segments (Name.unsafeParseText ".foo.bar")) (NameSegment "foo" :| [NameSegment "bar"])
      ok,
    scope "foo.`.`" do
      expectEqual' (isAbsolute (Name.unsafeParseText "foo.`.`")) False
      expectEqual' (segments (Name.unsafeParseText "foo.`.`")) (NameSegment "foo" :| [NameSegment "."])
      ok
  ]
