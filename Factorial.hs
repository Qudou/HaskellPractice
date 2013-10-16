module Factorial(factorial)
where

-- 階乗
fact :: Integer -> Integer
fact n
    | n <= 0    = 1
    | otherwise = n * (fact $ n - 1)

-- y!がxを超えない最大のyを返す。
maxFact :: Integer -> Integer
maxFact x = maxFact' x 1
    where
        maxFact' x y = if x >= fact y then maxFact' x $ y + 1 else y - 1

-- xの階乗進数を表すリストへの変換
factorial n = func n $ maxFact n
    where
        func 0 0 = []
        func x y = let (r, s) = quotRem x $ fact y in r:(func s $ y - 1)

