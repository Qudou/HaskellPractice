import Control.Applicative

module PowerSet (powerSet) where

powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = (++) <$> (powerSet xs) <*> [[x], []]
