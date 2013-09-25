import Control.Applicative

powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = (++) <$> (powerSet xs) <*> [[x], []]
