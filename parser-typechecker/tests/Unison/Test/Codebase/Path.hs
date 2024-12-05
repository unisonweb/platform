{-# LANGUAGE OverloadedLists #-}

module Unison.Test.Codebase.Path where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust)
import EasyTest
import Unison.Codebase.Path.Parse (parseHQName, parseHashOrHQName)
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name qualified as Name
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Prelude
import Unison.ShortHash qualified as SH

test :: Test ()
test =
  scope "path" . tests $
    [ scope "parseHashOrHQName" . tests $
        [ let s = "foo.bar#34"
           in scope s . expect $
                parseHashOrHQName s
                  == pure
                    ( pure
                        . HQ'.HashQualified (Name.fromReverseSegments $ NameSegment "bar" :| [NameSegment "foo"])
                        . fromJust
                        $ SH.fromText "#34"
                    ),
          let s = "foo.bar.+"
           in scope s . expect $
                parseHashOrHQName s
                  == pure
                    ( pure . HQ'.NameOnly . Name.fromReverseSegments $
                        NameSegment "+" :| [NameSegment "bar", NameSegment "foo"]
                    ),
          let s = "#123"
           in scope s . expect $
                parseHashOrHQName s == pure (Left . fromJust $ SH.fromText "#123")
        ],
      scope "parseHQ'Name" . tests $
        [ let s = "foo.bar#34"
           in scope s . expect $
                parseHQName s
                  == pure
                    ( HQ'.HashQualified (Name.fromReverseSegments $ NameSegment "bar" :| [NameSegment "foo"])
                        . fromJust
                        $ SH.fromText "#34"
                    ),
          let s = "foo.bar.+"
           in scope s . expect $
                parseHQName s
                  == pure
                    ( HQ'.NameOnly . Name.fromReverseSegments $
                        NameSegment "+" :| [NameSegment "bar", NameSegment "foo"]
                    ),
          let s = "#123" in scope s . expect $ isLeft $ parseHQName s
        ]
    ]
