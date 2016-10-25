module Structures.GeneralAux
where

import Data.List

removeDuplicates :: (Ord a, Eq a) => [a] -> [a]
removeDuplicates list = removeDuplicatesOrd $ sort list
removeDuplicatesOrd list = foldr f [] list
                           where
                                f a [] = [a]
                                f a list@(x:xs) 
                                                | a == x = list
                                                | otherwise = a:list

count :: (Eq a) => a -> [a] -> Int
count a list = foldr f 0 list
                where 
                f x ct | x == a = ct +1
                       | otherwise = ct 

productList :: [Double] -> Double
productList list = foldr f 1 list
                where
                f a b = a * b
                
getFromTupleList :: (Eq a) => a -> [(a,b)] -> b
getFromTupleList a [] = error "Not Found"
getFromTupleList a (l:ls) = let (x,y) = l
                            in
                                if a == x then y
                                else getFromTupleList a ls
                                
--returns the value in 1st list corresponding to the max value in the second list
maxRet1 :: (Ord b) => [a] -> [b] -> a
maxRet1 [] _ = error "empty list"
maxRet1 _ []  = error "empty list"

maxRet1 list1 list2 = fst $ foldr f first zipped
                      where 
                        zipped = zip list1 list2
                        f (x,y) (ix,iy) = if (y > iy) then (x,y) else (ix,iy)
                        first = zipped !! 0
                        
--maxRet2 ::(Ord a)=> [a] -> [b] -> b

-- split at w.r.t the char
split :: Char -> String -> [String]
split c str = fst (split' c ([],"") str)


split' char (list,str) [] 
			 | str /= "" =  (list ++ [str],"")
			 | otherwise = (list,"")
split' char (list,str) (f:fs) 
				| f == char = split' char (list++[str],"") fs
				| otherwise = split' char (list,str++[f]) fs


