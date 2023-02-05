module Main where

lista = [2,3,4,6,10,11,12]

paresMult3 lista = [a| a <- lista, even a, mod a 3 == 0]


cuboLista lista = [a^3| a <- lista]

quadrado(x) = x ^ 2
cubo(x) = x ^ 3

cuboImpares lista = [cubo(a)| a <- lista, odd a]

expLista lista = [cubo(a) + quadrado(b)| a <- lista, b <- lista, odd a, even b]
--main = print(paresMult3 [2,3,4,6,10,11,12])

--main = print(cuboImpares [2,3,4,6,10,11,12])

--main = print(cuboImpares [2,3,4,6,10,11,12])

main = print(expLista [2,3,4,5])