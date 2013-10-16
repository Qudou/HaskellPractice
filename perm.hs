module Perm (permuteAt) where

import Factorial

permuteAt x = transFactorialToPerm $ factorial x

transFactorialToPerm :: [Integer] -> [Integer]
transFactorialToPerm xs = func xs [0 .. (fromIntegral $ length xs)]
    where
        func :: [Integer] -> [Integer] -> [Integer]
        func _ [x] = [x]
        func (y:ys) s = let (itm, rest) = remove s y in itm:(func ys rest)
        remove :: [Integer] -> Integer -> (Integer, [Integer])
        remove zs n = let (former, (l:latter)) = splitAt (fromIntegral n) zs
                      in (l, former ++ latter)
