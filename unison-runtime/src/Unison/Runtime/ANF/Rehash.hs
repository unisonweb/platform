module Unison.Runtime.ANF.Rehash where

import Crypto.Hash
import Data.Bifunctor (bimap, second)
import Data.ByteArray (convert)
import Data.ByteString (cons)
import Data.ByteString.Lazy (toChunks)
import Data.Graph as Gr
import Data.List (foldl', nub, sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text (Text)
import Unison.Hash (fromByteString)
import Unison.Reference as Reference
import Unison.Referent as Referent
import Unison.Runtime.ANF as ANF
import Unison.Runtime.ANF.Serialize as ANF
import Unison.Symbol (Symbol)

checkGroupHashes ::
  [(Referent, Code)] ->
  Either (Text, [Referent]) (Either [Referent] [Referent])
checkGroupHashes rgs = case checkMissing rgs of
  Left err -> Left err
  Right [] ->
    case rehashGroups . Map.fromList $ bimap toReference codeGroup <$> rgs of
      Left err -> Left err
      Right (rrs, _) ->
        Right . Right . fmap (Ref . fst) . filter (uncurry (/=)) $ Map.toList rrs
  Right ms -> Right (Left $ Ref <$> ms)

rehashGroups ::
  Map.Map Reference (SuperGroup Symbol) ->
  Either (Text, [Referent]) (Map.Map Reference Reference, Map.Map Reference (SuperGroup Symbol))
rehashGroups m
  | badsccs <- filter (not . checkSCC) sccs,
    not $ null badsccs =
      Left (err, fmap (Ref . fst) . flattenSCC =<< badsccs)
  | otherwise = Right $ foldl step (Map.empty, Map.empty) sccs
  where
    err = "detected mutually recursive bindings with distinct hashes"
    f p@(r, sg) = (p, r, groupTermLinks sg)

    sccs = stronglyConnComp . fmap f $ Map.toList m

    step (remap, newSGs) scc0 =
      (Map.union remap rm, Map.union newSGs sgs)
      where
        rp b r
          | not b, Just r <- Map.lookup r remap = r
          | otherwise = r
        scc = second (overGroupLinks rp) <$> scc0
        (rm, sgs) = rehashSCC scc

checkMissing ::
  [(Referent, Code)] ->
  Either (Text, [Referent]) [Reference]
checkMissing (unzip -> (rs, cs)) = do
  is <- fmap Set.fromList . traverse f $ rs
  pure . nub . foldMap (filter (p is) . groupTermLinks . codeGroup) $ cs
  where
    f (Ref (DerivedId i)) = pure i
    f r@Ref {} =
      Left ("loaded code cannot be associated to a builtin link", [r])
    f r =
      Left ("loaded code cannot be associated to a constructor", [r])

    p s (DerivedId i) =
      any (\j -> idToHash i == idToHash j) s && not (Set.member i s)
    p _ _ = False

rehashSCC ::
  SCC (Reference, SuperGroup Symbol) ->
  (Map.Map Reference Reference, Map.Map Reference (SuperGroup Symbol))
rehashSCC scc
  | checkSCC scc = (refreps, newSGs)
  where
    ps = sortBy (comparing fst) $ flattenSCC scc
    sample = case fst $ head ps of
      Derived h _ -> h
      _ -> error "rehashSCC: impossible"
    bss = fmap (uncurry $ serializeGroupForRehash mempty) ps
    digest =
      hashFinalize $
        foldl'
          (\cx -> hashUpdates cx . toChunks)
          (hashInitWith Blake2b_256)
          bss
    newHash = fromByteString . cons 0 $ convert digest
    replace (Derived h i)
      | h == sample = Derived newHash i
    replace r = r

    replace' = overGroupLinks (\b r -> if b then r else replace r)

    newSGs = Map.fromList $ fmap (bimap replace replace') ps

    refreps = Map.fromList $ fmap (\(r, _) -> (r, replace r)) ps
rehashSCC scc = error $ "unexpected SCC:\n" ++ show scc

checkSCC :: SCC (Reference, a) -> Bool
checkSCC AcyclicSCC {} = True
checkSCC (CyclicSCC []) = True
checkSCC (CyclicSCC (p : ps)) = all (same p) ps
  where
    same (Derived h _, _) (Derived h' _, _) = h == h'
    same _ _ = False
