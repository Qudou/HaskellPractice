divList' :: [a] -> [a] -> [(a, [a])]
divList' _ [] = []
divList' former latter = let item = head latter
                            nFormer = former ++ [item]
                            nLatter = tail latter
                            rest = former ++ nLatter
                        in  (item, former ++ rest):(divList nFormer nLatter)

divList xs = divList' [] xs