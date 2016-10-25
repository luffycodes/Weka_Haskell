module C45.C45 (
             build
           , c45Dtree
           , classify
           ) where

import Structures.Structures
import Data.Ord
import Data.Maybe (fromJust)
import qualified Data.Map as Map hiding (partition)
--import Data.Map as Map
import qualified Data.List as List
--import Data.List as List

-- Support for missing attribute values not provided.
-- In case of continous valued (Numeric) attribute, only binary split is supported.
-- Expects attribute values of all data elements to be of type integer/double.

c45Dtree :: Int -> DataSetDT -> [FeatureDT] -> DataSetDT
c45Dtree _        []    _    = error "C45.c45Dtree: empty training set"
c45Dtree maxDepth train test = let
                                     dTree = build maxDepth train
                                     labels = map (classify dTree) test
                               in
                                   zipWith Labelled labels test

build :: Int -> [Labelled] -> DecisionTree
build _ [] = Leaf Nothing
build maxDepth dataset@(d:ds) = build' attbs maxDepth dataset
    where attbs = map fst . attributes $ point d

build' :: [Attribute] -> Int -> [Labelled] -> DecisionTree
build' _  maxDepth []      = Leaf Nothing --error $ "C45.build': empty dataset with depth left = " ++ show maxDepth
build' [] _ dataset = Leaf (Just $ majorityVote dataset)
build' _  0 dataset = Leaf (Just $ majorityVote dataset)
build' attbs maxDepth dataset = case gainRatio of
                                    0 -> Leaf (Just label)
                                    _ -> Node splitAttb child
    where
        (gainRatio,splitAttb) = findSplitAttb dataset attbs
        label = majorityVote dataset
        child = case splitAttb of
                  (Nominal _ _) -> (\val -> childLookup val childrenMap)
                                                where --childLookup v m = fromJust $ Map.lookup v m
                                                      childLookup v m = case Map.lookup v m of
                                                                            (Just c) -> c
                                                                            Nothing  -> Leaf $ Just "__NO-LABEL__"
                                                      datasetMap = splitCategories dataset splitAttb
                                                      childrenMap = Map.map (build' attbs' (maxDepth-1)) datasetMap
                                                      attbs' = List.delete splitAttb attbs
                  (Numeric _)   -> (\val -> findChild val)
                                                where findChild val | val<=splitValue = ftree
                                                                    | otherwise       = stree
                                                      (splitValue, (f,s)) = splitNumeric dataset splitAttb
                                                      ftree = build' attbs (maxDepth-1) f
                                                      stree = build' attbs (maxDepth-1) s

classify :: DecisionTree -> Datum -> Label
classify  (Leaf Nothing)             _             = "__NO-LABEL__"
classify  (Leaf (Just b))            _             = b
classify  (Node att f) dataPoint@(Datum list)      = classify (f attValue) dataPoint
    where attValue = getValue att dataPoint

findSplitAttb :: [Labelled] -> [Attribute] -> (Double,Attribute)
findSplitAttb []      _     = error "C45.findSplitAttb: empty dataset"
findSplitAttb dataset attbs = List.maximumBy compareByFst $ map (\a -> (splitGRatio dataset a,a)) attbs

-- | Computes the gain-ratio for splitting the dataset w.r.t the given attribute
splitGRatio :: [Labelled] -> Attribute -> Double
splitGRatio []      _                 = 0.0
splitGRatio dataset att@(Nominal _ _) = computeGainRatio dataset $ map snd $ Map.toList $ splitCategories dataset att
splitGRatio dataset att@(Numeric _)   = computeGainRatio dataset [f,s]
    where (_, (f,s)) = splitNumeric dataset att

computeGainRatio :: [Labelled] -> [[Labelled]] -> Double
computeGainRatio []      _     = 0.0
computeGainRatio _       []    = 0.0
computeGainRatio dataset parts = infoGain / splitInfo
    where infoGain = (entropy dataset) - sum (zipWith (*) pi es)
          splitInfo = (-1.0) * (sum $ zipWith (*) pi $ map (logBase 2) pi)
          es = map entropy parts
          n = (1.0::Double) * (fromIntegral $ length dataset)
          pi = map ((/n) . fromIntegral . length) parts

entropy :: [Labelled] -> Double
entropy []      = 0.0
entropy dataset = sum $ map (\x -> (-1.0::Double) * x * logBase 2 x) pi
    where pi = map ((/n) . fromIntegral . snd) $ countLabels dataset
          n = (1.0::Double) * (fromIntegral $ length dataset)

countLabels :: [Labelled] -> [(Label,Int)]
countLabels []      = []
countLabels dataset = Map.toList $ foldl (\m x -> Map.insertWith (+) (label x) 1 m) Map.empty dataset

splitCategories :: [Labelled] -> Attribute -> Map.Map Feature [Labelled]
splitCategories []      _                 = Map.empty
splitCategories dataset att@(Nominal _ _) = splitted
    where listMaps = map (\d -> Map.singleton (getValue att $ point d) [d]) dataset
          splitted = Map.unionsWith (++) listMaps
          --gainRatio = computeGainRatio dataset $ elems splitted

splitNumeric :: [Labelled] -> Attribute -> (Feature, ([Labelled],[Labelled]))
splitNumeric []      _               = error "C45.splitNumeric: empty dataset"
splitNumeric dataset att@(Numeric _) = (splitValue, List.partition (fun splitValue) sortedDataset)
    where splitValue = snd $ List.maximumBy compareByFst list
          fun val pt = (getValue att $ point pt) <= val
          list = map ( \c -> (split' att sortedDataset c,c) ) cands
          sortedDataset = List.sortBy bar dataset
          --bar x y = compare (getValue att $ point x) (getValue att $ point y)
          bar x y = Data.Ord.comparing (getValue att) (point x) (point y)
          cands = splitPointCandidates sortedDataset att
          --split' :: [Labelled a b] -> Attribute a -> a -> Double -> (Double, ([Labelled a b],[Labelled a b]))
          split' att dataset value = computeGainRatio dataset [l1, l2]
              where (l1, l2) = List.partition (fun value) dataset

splitPointCandidates :: [Labelled] -> Attribute -> [Feature]
splitPointCandidates []                _   = []
splitPointCandidates sortedList@(s:ss) att = removeDuplicatesFromSortedList $ foo (label s) sortedList [] att
    where foo _         []    cands  _       = cands
          foo currLabel (e:[]) cands att     = (getValue att $ point e) : cands
          foo currLabel (e1:e2:es) cands att = if (label e1)==currLabel
                                                   then foo currLabel (e2:es) cands att
                                                   else foo (label e1) (e2:es) ((getValue att $ point e2) : (getValue att $ point e1) : cands) att

removeDuplicatesFromSortedList :: Eq a => [a] -> [a]
removeDuplicatesFromSortedList = List.nub

majorityVote :: [Labelled] -> Label
majorityVote []      = error "C45.majorityVote: empty dataset"
majorityVote dataset = fst $ List.maximumBy compareBySnd $ countLabels dataset

getValue :: Attribute -> Datum -> Feature
getValue att d = fromJust $ lookup att $ attributes d

compareByFst :: Ord a => (a,b) -> (a,b) -> Ordering
--compareByFst = Data.Ord.comparing fst
compareByFst x y = compare (fst x) (fst y)

compareBySnd :: Ord b => (a,b) -> (a,b) -> Ordering
--comparebySnd x y = Data.Ord.comparing snd x y
compareBySnd x y = compare (snd x) (snd y)

