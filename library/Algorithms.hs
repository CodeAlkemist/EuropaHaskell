module Algorithms
    ( shatter
    , binarySearch
    ) where

shatter :: (Integral a) => a -> [a]
shatter n
  |n == 0 = []
  |n < 9 = [n]
  |n > 9 = shatter (n `div` 10) ++ [n `mod` 10]

binarySearch :: [Int] -> Int -> Int -> Int -> Int
binarySearch n value low high
  | null n = error "Can not search on empty list"
  | length n == 1 = head n
  | high < low = -1
  | nmid > value = binarySearch n value low (mid-1)
  | nmid < value = binarySearch n value (mid+1) high
  | otherwise = mid
  where mid = (low + high) `div` 2
        nmid = n !! mid
