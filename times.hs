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

 main :: [Time] -> IO b
 main times = do putStr"Digitando time\n"
                 time <- cadastrarTime
                 main (insert time times)

 masterMain = main []