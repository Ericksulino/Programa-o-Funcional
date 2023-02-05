module Main where

import System.Exit (exitSuccess)

multiplo2(n) = even n 

multiplo3(n) = mod n 3 == 0

menu = do
        putStrLn "1-Multiplo 2 "
        putStrLn "2-Multiplo 3"
        putStrLn "3-Sair"
        opcao <- getLine
        return (opcao)



principal("3") = exitSuccess   
principal(op)  =  do
                   opcao <- menu
                   caso(opcao)
                   principal(opcao)

  
principal1(op)  =  do
                   opcao <- menu
                   if (opcao /= "3")
                    then do
                          caso(opcao)
                          principal1(opcao)
                    else caso(opcao) 


principal2(op)
 | op == "3" = exitSuccess
 | otherwise =  do
                 opcao <- menu
                 caso(opcao)
                 principal2(opcao) 



caso "1"  = do
             putStrLn "Digite um nro: "
             nro <- getLine
             if multiplo2(read nro :: Int)
              then putStrLn "É multiplo 2 "
              else putStrLn "Nao é multiplo 2"

caso "2"  = do
             putStrLn "Digite um nro: " 
             nro <- getLine
             if multiplo3(read nro :: Int)
              then putStrLn "É multiplo 3 "
              else putStrLn "Nao é multiplo 3"

caso "3" = putStr "Obrigada por usar o sistema."

main = principal("0")