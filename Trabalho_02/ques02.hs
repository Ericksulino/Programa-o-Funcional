module Segunda where
import Data.List ( (\\) )

lista1 = [1,4,5,6,11,50,1690]
lista2 = [1,2,3,5,8,10,37,653]

--LETRA A:

confereAB(lista1,lista2) = (lista1 \\ lista2)
confereBA(lista1,lista2) = (lista2 \\ lista1)

letraA l1 l2 = lisuni(confereAB(l1,l2),confereBA(l1,l2))


--LETRA B:


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

ques02 :: [Int] -> [Int] -> IO ()
ques02 x y = do 
        putStr"a Para a alternativa A\n"
        putStr"b Para a alternativa B\n"
        putStr"0 para sair\n"
        putStr"-----> "
        op <-getLine
        
        if op == "0" then putStr"Finalizado com sucesso!\n"

        else do putStr"\n\n"
                escolha1(op, x, y)
                putStr"\n\n"
                ques02 x y
           
escolha1 :: (String, [Int], [Int]) ->  IO()
escolha1 (op, x, y)
 | op == "a" = putStr"\n">>print(letraA lista1 lista2 )
 | op == "b" = putStr"\n">>print(letraB x y)
 |otherwise = do putStrLn"Digite uma opção valida!"
                 ques02 x y

--uni duas listas de forma ordenada
lisuni :: Ord a => ([a], [a]) -> [a]
lisuni ([],[]) =[]
lisuni (l1,[]) = l1
lisuni ([],l2 )= l2
lisuni (c1:r1,c2:r2)
 |c1 == c2 = c1: lisuni(r1,r2)
 |c1 < c2 = c1: lisuni(r1,c2:r2)
 |otherwise = c2: lisuni(c1:r1,r2)
