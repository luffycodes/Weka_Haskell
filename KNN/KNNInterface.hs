module KNN.KNNInterface(
        crossValidate,
        traintest,
       makeObjectFromFile
)
where

import Data.List
import Structures.Structures
import Structures.GeneralAux
import Control.Applicative
import KNN.KNN
import Structures.Definitions
-- | Takes the data from a file and fits it with the given priortype and distribution to give IO (naive bayes object)
makeObjectFromFile :: Int -> DType -> Resolve -> FPath -> IO KNN
makeObjectFromFile k dt res fp = do 
                                        rawdata <- readFile fp
                                        let 
                                                dataset = convertToDataSet rawdata
                                        return $ fitKNN k dt res dataset
                                        





-- | converts the data in csv format to  DataSet
convertToDataSet :: String -> DataCollect
convertToDataSet str = datac
		where
			datac = breakup (split '\n' str)

breakup :: [String] -> [(FeatureVector,ClassName)]
breakup list = map breakup' list

breakup' :: String -> (FeatureVector, ClassName)
breakup' str = let 
                   all = split ',' str
                   features = take ((length all)-1) all
                   cls = last all
                   featurefinal = map (read) features
               in
                   (featurefinal,cls)
                   
--- end convertToDataSet
-- | converts the data in csv format to list of feature vector
convertToFeatureVectorSet :: String -> [FeatureVector]

convertToFeatureVectorSet str = let 
					firstsplit =  split '\n' str
					secondsplit = map (split ',') firstsplit
				in 
					map (map read) secondsplit



-- | trains the classifier on the train file and tests it on the test file writes in the file
traintest :: NeighbourNum -> DType -> Resolve -> TrainPath -> TestPath->  String -> IO ()

traintest k dtype res trainpath testpath clspath= 
							do
					   			rawtraindata <- readFile trainpath
								rawtestdata <- readFile testpath
								let
								  traindataset = convertToDataSet rawtraindata
								  testdataset = convertToFeatureVectorSet rawtestdata
								  predicted = knn k dtype res traindataset testdataset
								  (_,classes) = unzip predicted
								  classdata = intercalate "\n" classes
								writeFile clspath classdata




-- |crossValidates the given file and returns the performace of the classfier
crossValidate :: Double -> NeighbourNum -> DType -> Resolve -> FPath -> IO Performance
crossValidate ratio k dt res fp = do
					rawdata <- readFile fp
					let
						 datacollect = convertToDataSet rawdata
					return (crossValidateData ratio k dt res datacollect)

-- |crossValidate the data and returns the performace of the classifier
crossValidateData :: Double -> NeighbourNum -> DType -> Resolve -> DataCollect -> Performance

crossValidateData ratio k dt res datacollect = let
					                instances = splitDataSet ratio datacollect
					                results = (checkPerformance k dt res) <$> instances
					                (correct,total) = foldr f (0,0) results
							          where	
							        	f (_,(a,b)) (at,bt) = (a+at,b+bt)
				                        in 
					                (correct,total)







-- (traindataset, testdataset) -> (testdataset,performace)
-- | Trains the classifier on the first element of the tuple and tests it on the second dataset .. returns the performace
checkPerformance :: NeighbourNum -> DType -> Resolve -> (DataCollect,DataCollect) -> (DataCollect,Performance)
checkPerformance k dt res (trainC,testC) =  	let
                                                        test = fst <$> testC
			                                datapredicted = knn k dt res trainC test
						        ZipList eq = (==) <$> (ZipList datapredicted) <*> (ZipList testC)
						        count = length $ filter (\x->x==True) eq
					        
					        in
						(testC,(count,length testC))




-- making a list of tests
-- | splits the dataset into testing instances during crossvalidation
splitDataSet :: Double -> DataCollect -> [(DataCollect,DataCollect)]
splitDataSet ratio datacollect = let 
					size = decidesize ratio $ length datacollect
					count = gif (length datacollect)  size
					base = [0..(count-1)]
					start = (* size) <$> base
					end = (+(size-1)) <$> start
					slices = zip start end
			     in
					(slice datacollect) <$> slices

-- helper decidesize
decidesize r total = floor (r*(fromIntegral total))
				
-- greatest integer funcio
gif val div = ceiling ((fromIntegral val) / (fromIntegral div))



-- start >=0
-- end <= length -1
-- | takes out a slice from the Dataset
slice :: DataCollect -> (Int,Int) -> (DataCollect,DataCollect)

slice datacollect (start,end) 	| start >= (length datacollect) = error "sizes dont match"
				| otherwise = (dstrain,dstest)
				where
					dstrain1 = take start datacollect
					dstrain2 = drop (end +1) datacollect
					dstrain = dstrain1 ++ dstrain2
					dstest = take (end - start +1) $ drop start datacollect
					
					
main = do 
          rawdata <- readFile "./small.csv"
          let 
                dataset = convertToDataSet rawdata
          return (splitDataSet 0.5 dataset)
                
                
					
