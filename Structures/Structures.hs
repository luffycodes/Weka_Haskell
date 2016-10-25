{-| This particular file contains the auxilliary definitions of data that are required by different classifiers
-}

module Structures.Structures
where
--------------------------------------------------------------------------------------------------------Naive Bayes ---------------------------------------------------------------------------------------
-- |The name of the class.
type ClassName = String

-- |The feature vector
type FeatureVector = [Feature]

-- | Each of the feature is a double value, the interpretation will be based on the distribution type to be used.
type Feature = Double

-- | just better readability
type Probability = Double

{- | data set, i.e. the input. each of the feature vector is a point and the classes contain the corresponding class the particular feature 
 belongs to. The size of the points and the classes should be same
-}
data DataSet = DataSet { points :: [FeatureVector]
                         ,classes :: [ClassName]
                       }
		deriving (Show)

-- | Probe is basically a function that gives the probability of a feature having a particular value for a particular className
type Probe = ClassName -> Feature -> Probability

-- | this is same feature for different input points
type FeatureData = [Feature]
-- | this is the parallel data for classes corresponding to the input
type ClassData = [ClassName]

-- | Collection of feature vectors
type TestDataCollect = [FeatureVector]		--difference from DataCollect => no class names in this case 

{- | specifies the type of the distribution used to fit the training data for a particular feature
Source of Description : MathWork
Normal - Gaussian
Mvmn - Multivariate multinomial distribution 
Mn - Multinomial distribution 
-}
data DistributionType = Normal 
                   | Kernel 
                   | Mvmn 
                   | Mn 
                   deriving (Show)

data Distribution = Single DistributionType
                    | Multiple [DistributionType]
                    deriving (Show)   

{- | This defines which prior probablity to use.
        Empirical - Depends on the training data.
        Uniform  - Equal probablity for all classes
        Specific - User provided probability
        
-}
data PriorType = Empirical              -- depending on the training data
                | Uniform               -- equal
                | Specific [(ClassName,Probability)]         -- take as input
                deriving (Show)
-----------------------------------------------------------------------------------------------------------Knn ------------------------------------------------------------------------------------------

-- | data point, is the combination of point and its class
--data DataPoint = DataPoint { point::FeatureVector 
--							 ,className :: ClassName
--						   }
type DataPoint = (FeatureVector, ClassName)


-- | alternative to DataSet but in this classes or points are not directly separable
type DataCollect = [DataPoint]

-- | distance measure
--type DType = String
data DType =  Euclidean
            | Manhattan
            | DefaultDT
-- | Resolution method
data Resolve = Last
             | First
             | DefaultR

-- | The number of neighbours the KNN uses
type NeighbourNum = Int

-- | ROMIL
type ConflictControl = String



-----------------------------------------------------------------------------------------------------------DT -------------------------------------------------------------------------------------------

-- | NITESH

type Label = String
--type Feature = Double

data C45Dtree = C45Dtree { c45Classify :: Datum -> Label}

data DecisionTree = Leaf (Maybe Label)
                  | Node { attribute :: Attribute
                         , decide    :: Feature -> DecisionTree
                         --, children :: [DecisionTree a b]
                         }

newtype Datum = Datum { attributes :: [(Attribute, Feature)] }
                  deriving (Show)
data Labelled = Labelled { label::String, point::Datum }
                    deriving (Show)

type DataSetDT = [Labelled]
type FeatureDT = Datum

data Attribute = Nominal { attName::String, possibleValues::[Feature] }
               | Numeric { attName::String }
                 deriving (Show)

instance Eq Attribute where
    (==) (Nominal n1 l1) (Nominal n2 l2) = and [(==) n1 n2, (==) l1 l2]
    (==) (Numeric n1)   (Numeric n2)     = (==) n1 n2
    (==) _              _                = False
    
    
    
    
---------------------------------------------------------------------------------------------------------GEN ------------------------------------------------------------------------------------------
-- | General definition of Dataset to unify the interface
data DataSetGen = DT_NB DataSet
                 |DT_KNN DataCollect
                 |DT_DT DataSetDT
