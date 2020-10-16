module Lib
  ( someFunc,
  )
where

sum' :: [Integer] -> Integer
sum' xs = foldl (+) 0 xs

someFunc :: IO ()
someFunc = print "Learn you a Haskell for Great Good!"