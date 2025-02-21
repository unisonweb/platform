{-# LANGUAGE OverloadedLists #-}

module Unison.Test.Codebase.Path where

import Data.Maybe (fromJust)
import EasyTest
import Unison.Codebase.Path (Path' (..), Relative (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Path.Parse (parseHQSplit', parseHashOrHQSplit')
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Prelude
import Unison.ShortHash qualified as SH

test :: Test ()
test =
  scope "path" . tests $
    [ scope "parseHashOrHQSplit'" . tests $
        [ let s = "foo.bar#34"
           in scope s . expect $
                parseHashOrHQSplit' s
                  == pure
                    (pure . HQ'.HashQualified (relative ["foo"], NameSegment "bar") . fromJust $ SH.fromText "#34"),
          let s = "foo.bar.+"
           in scope s . expect $
                parseHashOrHQSplit' s == pure (pure $ HQ'.NameOnly (relative ["foo", "bar"], NameSegment "+")),
          let s = "#123"
           in scope s . expect $ parseHashOrHQSplit' s == pure (Left . fromJust $ SH.fromText "#123")
        ],
      scope "parseHQ'Name" . tests $
        [ let s = "foo.bar#34"
           in scope s . expect $
                parseHQSplit' s
                  == pure (HQ'.HashQualified (relative ["foo"], NameSegment "bar") (fromJust (SH.fromText "#34"))),
          let s = "foo.bar.+"
           in scope s . expect $ parseHQSplit' s == pure (HQ'.NameOnly (relative ["foo", "bar"], NameSegment "+")),
          let s = "#123" in scope s . expect $ isLeft $ parseHQSplit' s
        ]
    ]

relative :: [Text] -> Path'
relative = RelativePath' . Relative . Path.fromList . fmap NameSegment
