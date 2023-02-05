module Main where


--- remover

remover e [] = []
remover e (c:r)
 | e < c = c:r
 | e == c = r
 | otherwise = c: remover e r

