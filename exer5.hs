module Quinta where

  import Data.Char
  import Data.List

  import Control.Exception
  import System.IO
  import System.IO.Error
  import Prelude hiding (catch)

  type Matricula = Integer
  type Nome = String
  type Periodo = Int
  type Nota = Float
  type Codigo = Integer
  type Qtd_Periodos = Int

  type Curso = (Codigo, Nome, Qtd_Periodos)
  type Aluno = (Matricula, Nome, Curso, Periodo)
  type Disciplina = (Codigo, Codigo, Nome, Periodo)
  type Notas = (Matricula, Codigo, Nota, Nota)

  menu :: IO Int
  menu = do
    putStrLn "*****MENU*****"
    putStrLn " 0 - Finalizar Programa"
    putStrLn " 1 - Cadastrar Alunos"
    putStrLn " 2 - Cadastrar Curso "
    putStrLn " 3 - Cadastrar Disciplina"
    putStrLn " 4 - Cadastrar Notas"
    putStrLn " 5 - Visualizar alunos por curso"
    putStrLn " 6 - Visualizar alunos por período"
    putStrLn " 7 - Visualizar cursos"
    putStrLn " 8 - Disciplinas por curso"
    putStrLn " 9 - Disciplinas por periodo do curso"
    putStrLn "10 - Notas"
    putStr "Digite = "
    opcao <- getLine
    putStr "\n"
    return(read opcao :: Int)



    -- CADASTROS


  cadastro_curso ::  IO Cursos
  cadastro_curso = do
                    putStr "Codigo do curso: "
                    cdcur <-getLine
                    putStr "Nome do curso: "
                    nome <-getLine
                    putStr "Quantidade de Periodo: "
                    per <-getLine
                    dados <- le_arquivoAlunos
                    dadosAlunos <- (converteconteudo (dados))



  le_arquivoAlunos :: IO Cursos
  le_arquivoAlunos =  catch testa_arquivo trataErro
                   where
                    testa_arquivo = do
                                     {arq <-  openFile "dadosAlunos.txt" ReadMode;
                                      conteudo <- (hGetContents arq);
                                      return (conteudo);
                                      }
                    trataErro erro = if isDoesNotExistError erro
                                      then do
                                            {arq <- openFile "dadosAlunos.txt" WriteMode;
                                             hClose arq;
                                             le_arquivoAlunos;
                                             }
                                      else ioError erro


  converteconteudo :: String -> IO [[String]]
  converteconteudo [] = return [[]]
  converteconteudo conteudo = return (map (explodir '\n') ( explodir 'f' conteudo))

  explodir :: Eq a => a -> [a] -> [[a]]
  explodir a [ ] = [ ]
  explodir a (c:r)
  	| (takeWhile (/= a) (c:r)) == [ ] = explodir a r
  	| c == a = (takeWhile (/= a) r) : explodir a (dropWhile (/= a) r)
  	| otherwise = (takeWhile (/= a)(c:r)) : explodir a (dropWhile (/= a) (c:r))
