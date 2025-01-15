{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Util.Text where

import Control.Monad
import Data.List (foldl', unfoldr)
import Data.Text qualified as T
import EasyTest
import Unison.Util.Rope qualified as R
import Unison.Util.Text qualified as Text
import Unison.Util.Text.Pattern qualified as P

test :: Test ()
test =
  scope "util.text" . tests $
    [ scope "empty ==" $ expectEqual Text.empty Text.empty,
      scope "empty `compare`" . expectEqual EQ $ Text.empty `compare` Text.empty,
      scope "==" . expect $
        let a = join (replicate 100 ['a' .. 'z'])
            b = join (replicate 45 ['A' .. 'Z'])
         in (Text.pack a <> Text.pack b) == Text.pack (a ++ b),
      scope "at" $ do
        expectEqual' (Just 'a') $ Text.at 0 (Text.pack "abc")
        expectEqual' Nothing $ Text.at 0 mempty
        ok,
      scope "at.cornerCases" $ do
        let b = Text.drop 3 $ "abc" <> "def"
        expectEqual' (Just 'd') $ Text.at 0 b
        expectEqual' (Just 'a') $ Text.at 0 (mempty <> "abc")
        ok,
      scope "consistency with Text" $ do
        forM_ [(1 :: Int) .. 100] $ \_ -> do
          n <- int' 0 50
          m <- int' 0 50
          k <- int' 0 (n + m)
          o <- int' 0 50
          let ch = pick (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])
          t1 <- T.pack <$> replicateM n ch
          t2 <- T.pack <$> replicateM m ch
          t3 <- T.pack <$> replicateM o ch
          let (t1s, t2s, t3s) = (Text.fromText t1, Text.fromText t2, Text.fromText t3)
          scope "associativity" $ do
            -- note $ show (t1s, t2s, t3s)
            expect' $ t1s <> (t2s <> t3s) == (t1s <> t2s) <> t3s
          scope "<>" . expect' $
            Text.toText (t1s <> t2s <> t3s) == t1 <> t2 <> t3
          scope "Ord" . expect' $
            (t1 <> t2 <> t3) `compare` t3
              == (t1s <> t2s <> t3s) `compare` t3s
          scope "take" . expect' $
            Text.toText (Text.take k (t1s <> t2s)) == T.take k (t1 <> t2)
          scope "drop" . expect' $
            Text.toText (Text.drop k (t1s <> t2s)) == T.drop k (t1 <> t2)
          scope "uncons" . expect' $
            let ts = t1s <> t2s <> t3s
             in unfoldr Text.uncons ts == Text.unpack ts
          scope "unsnoc" . expect' $
            let ts = t1s <> t2s <> t3s
             in unfoldr (\t -> (\(ts, ch) -> (ch, ts)) <$> Text.unsnoc t) ts == reverse (Text.unpack ts)
          scope "at" $
            let bs = t1s <> t2s <> t3s
                b = t1 <> t2 <> t3
             in forM_ [0 .. (T.length b - 1)] $ \ind ->
                  expectEqual' (Just (T.index b ind)) $ Text.at ind bs
        ok,
      scope "lots of chunks" $ do
        forM_ [(0 :: Int) .. 25] $ \_ -> do
          n <- int' 0 50
          k <- int' 200 600
          chunks <- replicateM n (replicateM k char)
          let b1 = foldMap Text.pack chunks
              b2 = foldr (<>) mempty (Text.pack <$> chunks)
              b3 = foldl' (<>) mempty (Text.pack <$> chunks)
              b = T.concat (T.pack <$> chunks)
          expect' $ b1 == b2 && b2 == b3
          expectEqual' b $ Text.toText b1
          expectEqual' b $ Text.toText b2
          expectEqual' b $ Text.toText b3
        ok,
      scope "depth checks" $ do
        chunk <- Text.pack <$> replicateM 1000 char
        forM_ [100, 200, 400] $ \i0 -> do
          n <- int' 200 400
          i <- (i0 +) <$> int' (-10) 10
          let chunks = replicate i chunk
              t1 = foldMap id chunks
              t2 = foldr (<>) mempty chunks
              t3 = foldl' (<>) mempty chunks
              moarChunks = join (replicate n chunks)
              ts =
                [ t1,
                  t2,
                  t3,
                  foldMap id (replicate n t3),
                  foldr (<>) mempty moarChunks,
                  foldl' (<>) mempty moarChunks
                ]
              maxDepth = maximum depths
              depths = map depth ts
          note ("maximum depth for tree with " <> show (i * n) <> " chunks was " <> show maxDepth)
          expect' (maxDepth < log2 (i * n) * 2)
        ok,
      scope "patterns" $ do
        expectEqual' (Just ([], "")) $ P.run P.Eof ""
        expectEqual' (Just ([], "")) $ P.run (P.Char P.Any) "a"
        expectEqual' (Just ([], "")) $ P.run (P.Char (P.CharRange 'a' 'z')) "a"
        expectEqual' Nothing $ P.run (P.Char (P.Not (P.CharRange 'a' 'z'))) "a"
        expectEqual' (Just ([], "bc")) $ P.run (P.Or (P.Char (P.Not (P.CharRange 'a' 'z'))) (P.Char P.Any)) "abc"
        -- this shows that we ignore subcaptures
        expectEqual' (Just (["ab"], "def")) $
          P.run
            (P.Join [P.Capture (P.Join [P.Capture (P.Char P.Any), P.Capture (P.Char P.Any)]), P.Char P.Any])
            "abcdef"
        expectEqual' (Just ([], "ab")) $ P.run (P.Char (P.CharSet "0123")) "3ab"
        expectEqual' (Just ([], "3b")) $ P.run (P.Char (P.Not (P.CharSet "0123"))) "a3b"
        expectEqual' (Just (["a"], "3b")) $ P.run (P.Capture (P.Char (P.Not (P.CharSet "0123")))) "a3b"
        expectEqual' (Just ([], "123")) $ P.run (P.Many True (P.Char (P.CharSet "abcd"))) "babbababac123"
        expectEqual' (Just (["babbababac"], "123")) $
          P.run (P.Capture (P.Many True (P.Char (P.CharSet "abcd")))) "babbababac123"
        expectEqual' (Just (["012345"], "abc")) $
          P.run (P.Capture (P.Many True (P.Char (P.CharClass P.Number)))) "012345abc"
        expectEqual' (Just (["012345", "abc"], "")) $
          P.run
            ( P.Join
                [ P.Capture (P.Many True (P.Char (P.CharClass P.Number))),
                  P.Literal ",",
                  P.Capture (P.Many True (P.Char P.Any))
                ]
            )
            "012345,abc"
        expectEqual' (Just (["01", "10", "20", "1123", "292", "110", "10"], "")) $
          P.run
            ( P.Many
                True
                ( P.Join
                    [ P.Capture (P.Many True (P.Char (P.CharClass P.Number))),
                      P.Many True (P.Char (P.CharClass P.Whitespace))
                    ]
                )
            )
            "01 10 20 1123 292 110 10"
        expectEqual' (Just (["127", "0", "0", "1"], "")) $
          let part = P.Capture (P.Replicate 1 3 (P.Char (P.CharClass P.Number)))
              dpart = P.Join [P.Literal ".", part]
              ip = P.Join [part, P.Replicate 3 3 dpart, P.Eof]
           in P.run ip "127.0.0.1"
        expectEqual' (Just (["1", "2", "3", "4", "5"], "")) $
          let p = P.Replicate 5 8 (P.Capture (P.Char (P.CharClass P.Number)))
           in P.run p "12345"
        expectEqual' (Just ([], "1234")) $
          let p = P.Replicate 5 8 (P.Capture (P.Char (P.CharClass P.Number))) `P.Or` P.Join []
           in P.run p "1234"
        expectEqual' (Just (["1z", "2z", "3z", "4z", "5z"], "6a")) $
          let p = P.Replicate 5 8 (P.Capture (P.Join [P.Char (P.CharClass P.Number), P.Literal "z"])) `P.Or` P.Join []
           in P.run p "1z2z3z4z5z6a"
        -- https://github.com/unisonweb/unison/issues/3530
        expectEqual Nothing $
          let p =
                P.Or
                  (P.Join [P.Literal "a", P.Literal "b"])
                  (P.Join [P.Literal "a", P.Literal "c"])
           in P.run p "aac"
        expectEqual (Just ([], "")) $
          let p =
                P.Or
                  ( P.Capture $
                      ( P.Or
                          (P.Join [P.Literal "a", P.Literal "b"])
                          (P.Join [P.Literal "a", P.Literal "c"])
                      )
                  )
                  (P.Join [P.Literal "aa", P.Literal "cd"])
           in P.run p "aacd"
        -- this is just making sure we don't duplicate captures to our left
        -- when entering an `Or` node
        expectEqual (Just (["@"], "")) $
          let p = P.Join [P.Capture (P.Char P.Any), P.Or (P.Literal "c") (P.Join []), P.Literal "d"]
           in P.run p "@cd"
        expectEqual (Just (["%", "c"], "")) $
          let p = P.Join [P.Capture (P.Char P.Any), (P.Or (P.Capture (P.Literal "c")) (P.Join [])), P.Literal "d"]
           in P.run p "%cd"
        expectEqual (Just ([""], "ac")) $
          let p = P.Capture (P.Or (P.Join [P.Literal "a", P.Literal "b"]) (P.Join []))
           in P.run p "ac"
        expectEqual (Just ([""], "ac")) $
          let p = P.Capture (P.Replicate 0 1 (P.Join [P.Literal "a", P.Literal "b"]))
           in P.run p "ac"
        -- nested or tests
        expectEqual (Just (["zzzaaa", "!"], "!!")) $
          let p =
                P.Or
                  ( P.Or
                      (P.Literal "a")
                      (P.Join [P.Literal "z", P.Replicate 3 5 (P.Literal "z")])
                  )
                  (P.Join [P.Capture (P.Literal "zzzaaa"), P.Capture (P.Literal "!")])
           in P.run p "zzzaaa!!!"
        ok,
      scope "ordinal" do
        let expectOrdinal = \ord -> expectEqual @String ord . Text.ordinal
        expectOrdinal "1st" 1
        expectOrdinal "2nd" 2
        expectOrdinal "3rd" 3
        expectOrdinal "4th" 4
        expectOrdinal "5th" 5
        expectOrdinal "10th" 10
        expectOrdinal "11th" 11
        expectOrdinal "12th" 12
        expectOrdinal "13th" 13
        expectOrdinal "14th" 14
        expectOrdinal "21st" 21
        expectOrdinal "22nd" 22
        expectOrdinal "23rd" 23
        expectOrdinal "24th" 24
        expectOrdinal "111th" 111
        expectOrdinal "112th" 112
        expectOrdinal "113th" 113
        expectOrdinal "121st" 121
        expectOrdinal "122nd" 122
        expectOrdinal "123rd" 123
    ]
  where
    log2 :: Int -> Int
    log2 n
      | n <= 1 = 0
      | otherwise = 1 + log2 (div n 2)
    depth (Text.Text t) = R.debugDepth t
