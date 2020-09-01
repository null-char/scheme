module Lib
  ( someFunc,
  )
where

import Data.List

doubleMe x = x * x

-- Another way of multipling two numbers I guess
x :: (Num a) => a -> a -> a
x a b = a * b

-- Checks to see if an element is in a given list
elem' x xs =
  if null xs
    then False
    else
      if x == (xs !! 0)
        then True
        else x `elem'` drop 1 xs

flatten' :: [[a]] -> [a] -> [a]
flatten' xxs acc = do
  if null xxs
    then acc
    else do
      let flattened = [[x | x <- xs] | xs <- take 1 xxs] !! 0
      flatten' (drop 1 xxs) $ acc ++ flattened

flatten :: [[a]] -> [a]
flatten xxs = flatten' xxs []

zip'' :: [a] -> [b] -> [(a, b)] -> [(a, b)]
zip'' l1 l2 acc = do
  if null l1 || null l2
    then acc
    else zip'' (tail l1) (tail l2) (acc ++ [(head l1, head l2)])

-- My implementation of zip. This function takes in two lists and zips them into one list with each
-- element being a corresponding tuple pair
zip' :: [a] -> [b] -> [(a, b)]
zip' [] ys = []
zip' xs [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

max' a b
  | a > b = a
  | otherwise = b

myComp' a b
  | a > b = GT
  | a < b = LT
  | otherwise = EQ

pingBangsBoom xs = [if x < 20 then "PING" else if x > 20 && x < 50 then "BANG" else "BOOM" | x <- xs]

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * (factorial $ n - 1)

third :: (a, b, c) -> c
third (_, _, c) = c

add3DVector :: (Num x, Num y, Num z) => (x, y, z) -> (x, y, z) -> (x, y, z)
add3DVector v1 v2 =
  let (x1, y1, z1) = v1
      (x2, y2, z2) = v2
   in (x1 + x2, y1 + y2, z1 + z2)

emptyListErr = error "Oops! You can't really access an element from an empty list"

head' [] = emptyListErr
head' (x : rest) = x

tail' [] = emptyListErr
tail' (_ : rest) = rest

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

someFunc :: IO ()
someFunc = print "Learn you a Haskell for Great Good!"