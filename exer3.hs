module Terceira where

  import Data.List(insert)

  type Nome = String
  type UF = String
  type Pais = String
  type Ano = Integer
  type Qtd_Periodos = Int

  type Time = (Nome,UF,Pais,Ano)

  cadastrarTime ::  IO Time
  cadastrarTime = do

                    putStr "Nome do Time: "
                    nome <-getLine
                    putStr "Estado: "
                    uf <-getLine
                    putStr "Pais: "
                    pais <-getLine
                    putStr "Ano de Fundação: "
                    ano <-getLine
                    putStr "\nDados Cadastrados com Sucesso!\n\n"
                    return (nome, uf,pais,read ano)

  quicksort :: (Ord a) => [a] -> [a]
  quicksort [] = []
  quicksort (x:xs) =
      let smallerSorted = quicksort [a | a <- xs, a <= x]
          biggerSorted = quicksort [a | a <- xs, a > x]
      in  smallerSorted ++ [x] ++ biggerSorted


  busca :: Eq b1 => ([(b1, b2, c, d)], b1) -> [(b1, b2, c, d)]
  busca([],nm) = []
  busca((nme,uf,p,an):r,nm)
   |nme == nm = [(nme,uf,p,an)]
   |otherwise = busca(r,nm)


  mostar :: Show a => [a] -> IO ()
  mostar [] = putStr "\n"
  mostar (c: r) = print (c)>>mostar r


  main :: [Time] -> IO ()
  main times = do
                  putStrLn"\n--MENU--\n"
                  putStr "C-Cadastrar Time\nL-Lista\nO-Lista ordenada\nB-Buscar informações\n0-Sair\n"
                  putStr"----> "
                  num <- getLine
                  if num == "0"
                     then putStrLn"Finalizado com sucesso!"
                  else
                   if num == "C"||num=="c"
                      then do putStr"\nDigitando time\n"
                              time <- cadastrarTime
                              main (insert time times)
                   else if num =="b"||num=="B"
                    then do putStr"Digite o nome do time: "
                            nme<-getLine
                            putStr"\nTime pesquisado:\n\n"
                            print (busca(times,nme))
                            putStr("\n")
                            main times

                       else if num == "O"||num=="o"
                              then do
                                   print(quicksort(times))
                                   putStr("\n")
                                   main times
                            else if num == "L"||num=="l"
                                    then do putStrLn"TIMES:"
                                            mostar times
                                            main times
                                 else do putStrLn"Digite uma opção valida!"
                                         main times

  principal :: IO ()
  principal = main []
