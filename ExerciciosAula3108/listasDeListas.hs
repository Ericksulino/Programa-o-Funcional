module Main where

-- lista = [[1,2,3,4],[4,5,6,7],[7,8,9,10]]



pertence e [] = False
pertence e (c:r)
  | e < c = False
  | e == c = True
  | otherwise = pertence e r

qtdeListas e [] = 0
qtdeListas e (l:rl)
  | pertence e l = 1 + qtdeListas e rl
  | otherwise = qtdeListas e rl


-- lista = [[1,2,3,4],[4,5,6,7],[7,8,9,10]]

paresLista [] = []
paresLista (c:r)
 | even c = c: paresLista r
 | otherwise = paresLista r


paresListas [] = []
paresDeListas (l:rl) = paresLista l : paresListas rl


paresListasLC lista = [a| l <- lista, a <- l, even a]