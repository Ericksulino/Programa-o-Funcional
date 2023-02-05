
--Questão 1:
-- Para a execução das funções é necessario digitar "le" com a letra e o número correspondente
-- de cada função seguido das duas listas para executar cada letra por vez.Ex: leA1 [1,2,3,4] [4,6,7,8]
-- ou da melhor forma que é executar a função ques01 seguido das duas listas, onde tem um menu para a
-- escolha da função a ser usada. Ex: ques01 [1,2,3,4,5,6,7,8,9] [10,11,12,13,14,15]

module ExercicioQuatro where

import Data.Char ( toLower )

leA1 :: (Num a, Foldable t1, Foldable t2) => t1 a -> t2 a -> a
leA1 a b = sum a + sum b


leB1 :: (Num a, Ord a) => [a] -> [a] -> [a]
leB1 a b = map(^2)a++map(^2)b



leC1 :: (Ord a, Foldable t1, Foldable t2) => t1 a -> t2 a -> a
leC1 a b
 | maximum a >maximum b = maximum a
 |otherwise = maximum b



leD1 :: Integral a => [a] -> [a] -> [a]
leD1 a b = multi(a,3)++multi(b,3)


leE1 :: Num a => [a] -> [a] -> Int -> a
leE1 a b n= x * y
           where x= a!!n
                 y= b!!n


leF1 :: Integral a => [a] -> [a] -> [a]
leF1 a b = multi(map(^2)a,5)++multi(map(^3)b,5)


multi :: Integral a => ([a], a) -> [a]
multi([],m) = []
multi(c:r,m)
 | mod c m == 0 = c: multi(r,m)
 |otherwise = multi(r,m)


--lisuni :: Ord a => ([a], [a]) -> [a]
--lisuni ([],[]) =[]
--lisuni (l1,[]) = l1
--lisuni ([],l2 )= l2
--lisuni (c1:r1,c2:r2)
-- |c1 == c2 = c1: lisuni(r1,r2)
-- |c1 < c2 = c1: lisuni(r1,c2:r2)
-- |otherwise = c2: lisuni(c1:r1,r2)

ques01 :: [Int] -> [Int] -> IO ()
ques01 x y = do 
        putStr"a Somatório das Listas\n"
        putStr"b Quadrado das Listas\n"
        putStr"c Maior elemento das Listas\n"
        putStr"d Múltiplos de 3 das Listas\n"
        putStr"e Produto da Posição das Listas\n"
        putStr"f Múltiplos de 5 do Lista1ˆ2 d Lista2ˆ3\n"
        putStr"0 para sair\n"
        putStr"-----> "
        op <-getLine
        
        if op == "0" then putStr"Finalizado com sucesso!\n"

        else do putStr"\n\n"
                escolha1(op, x, y)
                putStr"\n\n"
                ques01 x y
           
escolha1 :: (String, [Int], [Int]) ->  IO()
escolha1 (op, x, y)
 | op == "a" = putStr"Somatório das Listas:\n">>print(leA1 x y)
 | op == "b" = putStr"Quadrado das Listas:\n">>print(leB1 x y)
 | op == "c" = putStr"Maior elemento das Listas:\n">>print(leC1 x y)
 | op == "d" = putStr"Múltiplos de 3 das Listas:\n">>print(leD1 x y)
 | op == "e" = do putStr "digite a posição: "
                  p <- getLine
                  putStr"Múltiplos de 5 do Lista1ˆ2 d Lista2ˆ3:\n">>print(leE1 x y (read p))
 | op == "f" = print (leF1 x y)
 |otherwise = do putStrLn"Digite uma opção valida!"
                 ques01 x y

--Questão 2:
-- Para a execução das funções é necessario digitar "le" com a letra e o número correspondente
-- de cada função seguido das duas listas para executar cada letra por vez.Ex: leA2 [1,2,3,4] [4,6,7,8]
-- ou da melhor forma que é executar a função ques01 seguido das duas listas, onde tem um menu para a
-- escolha da função a ser usada. Ex: ques02["programação","funcional","ufpi","sistemas"]

leA2 :: Foldable t => [t a] -> [Int]
leA2 [] = []
leA2(c:r)=length c: leA2(r)

leB2 :: (Foldable t, Ord a) => t a -> a
leB2 a = minimum a

leC2 :: (Foldable t, Ord a) => t [a] -> [a]
leC2 a = maximum a ++ minimum a


leD2 :: [[Char]] -> [[Char]]
leD2 lis = filter(\x -> vogais(toLower(head x)))lis

vogais :: Char -> Bool
vogais(c)
 |c == 'a'|| c == 'e'|| c == 'i'|| c == 'o'||c == 'u' = True
 |otherwise = False

ques02 :: [[Char]] -> IO ()
ques02 x = do putStr"a Tamanho das Strings.\n"
              putStr"b Menor string.\n"
              putStr"c União Maior e Menor.\n"
              putStr"d Strings inicial com vogal.\n"
              putStr"0 para sair\n"
              putStr"-----> "
              op <-getLine

              if op == "0" then putStr"Finalizado com sucesso!\n"

              else do putStr"\n\n"
                      escolha2(op,x)
                      putStr"\n\n"
                      ques02 x

escolha2 :: ([Char], [[Char]]) -> IO ()
escolha2 (op, x)
 | op == "a" = putStr"Tamanho das Strings:\n">>print(leA2 x)
 | op == "b" = putStr"Menor string:\n">>print(leB2 x)
 | op == "c" = putStr"União Maior e Menor:\n">>print(leC2 x)
 | op == "d" = putStr"Strings inicial com vogal:\n">>print(leD2 x)
 |otherwise = do putStrLn"Digite uma opção valida!"
                 ques02 x

--Ques03

valicpf :: Foldable t => t a -> Bool
valicpf cpf = length cpf == 11

dataInt :: String -> Int
dataInt date = read date :: Int

ano :: [Char] -> Int
ano date = mod dat 10000
           where dat = dataInt date

mes :: [Char] -> Int
mes date = mod dat 100
           where da = dataInt date
                 dat = div da 10000
dia :: [Char] -> Int
dia date = mod dat 10000
           where da = dataInt date
                 dat = div da 1000000
nasc :: [Char] -> Bool
nasc date
 | ano date >2020 = False
 | dia date > 31 = False
 | mes date >12 = False
 | mes date == 2 && dia date > 28 = False
 | mes date == 11 && dia date > 30 = False
 | otherwise = True



tuteste :: [([Char], [Char], [Char], Char)]
tuteste = [("12345678901","Jose da silva","12111989",'M'),("1098765432","Maria da silva","01011995",'F')]


ques03 :: Foldable t => [(t a, b, [Char], d)] -> [Char]
ques03((c,no,id,s):r)
 | valicpf c == True = "Cpf Valido!"
 | valicpf c == False = "Cpf Invalido"
 | nasc id == True  = "Data valida!"
 | nasc id == False = "Data invalida!"



pesquisa :: Eq a1 => ([([a1], [a2], [a3], [Char])], [a1]) -> [Char]
pesquisa ([],cpf) = "Pessoa não encontrada!"
pesquisa((c,no,id,s):r,cpf)
 | cpf == c = do "Pessoa encontrada!\n"
                 "CPF:">>c
                 "Nome:">>no
                 "Nascimento:">>id
                 "Sexo:">>s
 | otherwise = pesquisa(r,cpf)

teste = ques03 tuteste