module Main where

--- lista = []
-- lista = [10,20,25,35,40]
-- lista = (c:r)
-- lista = (a:b:r)
--x = 25

pertence(x,[]) = False
pertence(x,c:r)
 | x == c = True
 | otherwise = pertence(x,r)


contaElem([], qtde) = qtde
contaElem(c:r,qtde) = contaElem(r,qtde+1)

contaElemP([]) = 0
contaElemP(c:r) = 1+ contaElemP(r)


contaElemPar([]) = 0
contaElemPar(c:r) 
 | even c = 1 + contaElemPar(r)
 |otherwise = contaElemPar(r)


contaElemPosPar([]) = 0
contaElemPosPar([a]) = 0
contaElemPosPar(a:b:r) = 1 + contaElemPosPar(r)

impLista [] = putStr ""
impLista(c:r) = do
				 print c
				 impLista r
				

impListaInv [] = putStr ""
impListaInv(c:r) = do
					impListaInv r 
					print c
   

nPrimeirosElem(n,c:r)
 | n ==0 = []
 | otherwise = c : nPrimeirosElem(n-1,r)            


duplicarElem([]) = []
duplicarElem(c:r) = 2*c : duplicarElem r


duplicaLista l = [2*c | c <- l]

produtoCartesiano l1 l2 = [(a,b) | a<- l1, b<-l2]

main = print(contaElemP("Aula"))