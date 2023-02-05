module Main where

leint mensagem = do
            putStr mensagem
            linha <- getLine
            return (read linha :: Int) 
            
soma x y = return(x+y)

main = do
        x <- leint "nro1: "
        y <- leint "nro2: "
        s <- soma x y
        putStr "O valor da soma é: "
        print (s)

            
            