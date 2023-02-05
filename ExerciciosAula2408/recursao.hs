module Main where

--Faça uma função em Haskell que dado um intervalo devolva a soma entre o cubo dos números pares e o quadrado dos números ímpares sem considerar as extremidades.

-- inicio =  2 fim = 10

-- i = inicio +1 = 3

-- soma = 

somaNro(i,f,soma)
 | i == f = soma
 | i < f && even i = somaNro(i+1,f,soma+(i^3))
 | i < f && odd i = somaNro(i+1,f,soma+(i^2))
 | otherwise = somaNro(f,i,soma)

somaNroP(i,f)
 | i == f = 0
 | i < f  && even i = (i^3) + somaNroP(i+1,f)
 | i < f  && odd i = (i^2) + somaNroP(i+1,f)
 | otherwise = somaNroP(f,i)

main = do
        putStrLn "I: "
        i <- getLine
        putStrLn "F: "
        f <- getLine
        print(somaNro((read i)+1,read f,0))
        