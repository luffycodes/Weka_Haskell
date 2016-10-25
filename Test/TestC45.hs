module Test.TestC45 where

import C45.C45Interface
import Structures.Structures

a1 = Numeric "a1"
a2 = Numeric "a2"
a3 = Numeric "a3"
a4 = Numeric "a4"
a5 = Numeric "a5"

attbs = [a1,a2,a3,a4,a5]

main = do
           per <- crossValidate 0.5 4 attbs "./Test/small.csv"
           putStrLn $ show per
