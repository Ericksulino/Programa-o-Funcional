module Segunda where

  import Data.List


  lista1 = [1,4,5,6,11,50,1690]
  lista2 = [1,2,3,5,8,10,37,653]

  -- letra a
  confereAB(lista1,lista2) = (lista1 \\ lista2)
  confereBA(lista1,lista2) = (lista2 \\ lista1)

  -- letra b

  prim :: Num a => [a] -> a
  prim a = sum (map(^3) (take 2 a))

  maiores :: (Ord a, Num a) => [a] -> [a]
  maiores a = filter(>prim a)a

  elev :: (Ord b, Num b) => [b] -> [b]
  elev a = map(^2)x
           where x = maiores a

  soma :: Num a => ([a], [a]) -> [a]
  soma ([],[]) =[]
  soma (l1,[]) = l1
  soma ([],l2 )= l2
  soma (c1:r1,c2:r2) = c1+c2: soma(r1,r2)

  letraB :: (Num a, Ord a) => [a] -> [a] -> [a]
  letraB a b = soma(elev a,elev b)


  main = do
    print(confereAB(lista1,lista2))
    print(confereBA(lista1,lista2))
    print(letraB lista1 lista2)
