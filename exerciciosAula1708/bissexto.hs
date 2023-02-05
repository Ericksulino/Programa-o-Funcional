module Main where

bissexto n 
    |n `mod` 4 ==0  && n /= 2000 = True
    |otherwise  = False


bissexto1 n
    |mod n 4 == 0 && n /= 2000 = putStr "Eh Bissexto"
    |otherwise  = putStr " Nao eh Bissexto"


main = do
        putStrLn "Digite um Ano: "
        ano <- getLine
        if bissexto (read ano :: Int)
            then putStr "Eh bissexto"
            else putStr "Nao eh bissexto"


