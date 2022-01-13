-- Exercise set 4b: folds

module Set4b where

import Mooc.Todo
import Debug.Trace
------------------------------------------------------------------------------
-- Ex 1: countNothings with a fold. The function countNothings from
-- the course material can be implemented using foldr. Your task is to
-- define countHelper so that the following definition of countNothings
-- works.
--
-- Hint: You can start by trying to add a type signature for countHelper.
--
-- Challenge: look up the maybe function and use it in countHelper.
--
-- Examples:
--   countNothings []  ==>  0
--   countNothings [Just 1, Nothing, Just 3, Nothing]  ==>  2

countNothings :: [Maybe a] -> Int
countNothings xs = foldr countHelper 0 xs

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- countHelper's second arg is the accumulator

countHelper :: Maybe a -> Int -> Int
countHelper Nothing i = i + 1
countHelper _ i = i


------------------------------------------------------------------------------
-- Ex 2: myMaximum with a fold. Just like in the previous exercise,
-- define maxHelper so that the given definition of myMaximum works.
--
-- Examples:
--   myMaximum []  ==>  0
--   myMaximum [1,3,2]  ==>  3

myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = foldr maxHelper x xs

-- maxHelper :: Int -> Int -> Int
-- maxHelper a b
--  | a >= b = a
--  | otherwise = b
-- or we could just:
maxHelper = max

------------------------------------------------------------------------------
-- Ex 3: compute the sum and length of a list with a fold. Define
-- slHelper and slStart so that the given definition of sumAndLength
-- works. This could be used to compute the average of a list.
--
-- Start by giving slStart and slHelper types.
--
-- Examples:
--   sumAndLength []             ==>  (0.0,0)
--   sumAndLength [1.0,2.0,4.0]  ==>  (7.0,3)


sumAndLength :: [Double] -> (Double,Int)
sumAndLength xs = foldr slHelper slStart xs

slStart = (0.0, 0)
slHelper :: Double -> (Double, Int) -> (Double, Int)
slHelper d (s, len) = (s + d, len + 1)

------------------------------------------------------------------------------
-- Ex 4: implement concat with a fold. Define concatHelper and
-- concatStart so that the given definition of myConcat joins inner
-- lists of a list.
--
-- Examples:
--   myConcat [[]]                ==> []
--   myConcat [[1,2,3],[4,5],[6]] ==> [1,2,3,4,5,6]

myConcat :: [[a]] -> [a]
myConcat xs = foldr concatHelper concatStart xs

-- !!@ bob why does this work and not stack them backwards?
-- concatHelper's second argument should be the accumulator right?
--   As in countNothings above?

-- But it's acting here like the first arg is the accumulator. 
-- We have [], and each step adds its contribution to the end
-- why then is it xs ++ accum and not accum ++ xs?
concatStart = []
concatHelper :: [a] -> [a] -> [a]
concatHelper [] accum = accum
concatHelper xs accum = xs ++ accum 


------------------------------------------------------------------------------
-- Ex 5: get all occurrences of the largest number in a list with a
-- fold. Implement largestHelper so that the given definition of largest works.
--
-- Examples:
--   largest [] ==> []
--   largest [1,3,2] ==> [3]
--   largest [1,3,2,3] ==> [3,3]

largest :: [Int] -> [Int]
largest xs = foldr largestHelper [] xs

largestHelper :: Int -> [Int] -> [Int]
largestHelper x [] = [x]
largestHelper x accum
  | x > (head accum) = [x]
  | x == (head accum) = accum ++ [x]
  | otherwise = accum

------------------------------------------------------------------------------
-- Ex 6: get the first element of a list with a fold. Define
-- headHelper so that the given definition of myHead works.
--
-- Start by giving headHelper a type.
--
-- Examples:
--   myHead []  ==>  Nothing
--   myHead [1,2,3]  ==>  Just 1

myHead :: [a] -> Maybe a
myHead xs = foldr headHelper Nothing xs

headHelper ::  a -> Maybe a -> Maybe a
-- !!@ bob no idea why this doesn't keep calling the helper. 
headHelper x _ = trace ("hey") Just x
-- !!@ no idea why this one chooses the last thing in the list
--headHelper _ (Just x) = trace ("hey") Just x

------------------------------------------------------------------------------
-- Ex 7: get the last element of a list with a fold. Define lasthelper
-- so that the given definition of myLast works.
--
-- Start by giving lastHelper a type.
--
-- Examples:
--   myLast [] ==> Nothing
--   myLast [1,2,3] ==> Just 3

myLast :: [a] -> Maybe a
myLast xs = foldr lastHelper Nothing xs

--lastHelper = todo
lastHelper :: a -> Maybe a -> Maybe a
lastHelper x Nothing = Just x
lastHelper _ accum = accum

-- myHead :: [a] -> Maybe a
-- myHead xs = foldr headHelper Nothing xs


