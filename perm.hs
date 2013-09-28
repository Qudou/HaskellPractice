module Perm (perm) where

import Control.Monad
import Control.Applicative

perm :: Eq a => [a] -> [[a]]
perm [] = [[]]
perm xs = let
            lmd x = (x:) <$> (perm $ remove x xs)
            remove y ys = filter (/= y) ys
           in
            xs >>= lmd
