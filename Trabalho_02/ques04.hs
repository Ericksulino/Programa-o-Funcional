module Quarta where

  import Data.List(insert)

  type Matricula = Integer
  type Nome = String
  type Periodo = Int
  type Nota = Float
  type Codigo = Integer
  type Qtd_Periodos = Int

  type Cursos = (Codigo, Nome, Qtd_Periodos)
  type Alunos = (Matricula, Nome, Codigo, Periodo)
  type Disciplinas = (Codigo, Codigo, Nome, Periodo)
  type Notas = (Matricula, Codigo, Nota, Nota)


 --Mostrar na tela

  visualizar :: Show a => [a] -> IO ()
  visualizar [] = putStr "\n"
  visualizar (c:r) = print(c)>>visualizar r

  --VERIFICAÇÕES



  -- BUSCAS


  buscar_alunos :: Eq b1 => ([(a, b2, b1, d)], b1) -> [(a, b2, b1, d)]
  buscar_alunos([],codig) = []
  buscar_alunos((mt,nm,cod,per):r,codig)
   |cod == codig = (mt,nm,cod,per): buscar_alunos(r,codig)
   |otherwise = buscar_alunos(r,codig)


  busca_per :: (Eq b1, Eq c) => ([(a, b2, b1, c)], b1, c) -> [(a, b2, b1, c)]
  busca_per([],codig,peri)=[]
  busca_per((mt,nm,cod,per):r,codig,peri)
   |cod == codig && per == peri = (mt,nm,cod,per): busca_per(r,codig,peri)
   |otherwise = busca_per(r,codig,peri)


  buscar_disc :: Eq b => ([(a, b, c, d)], b) -> [(a, b, c, d)]
  buscar_disc([],codig) = []
  buscar_disc((cdd,cdc,nm,per):r,codig)
   |cdc == codig = (cdd,cdc,nm,per): buscar_disc(r,codig)
   |otherwise = buscar_disc(r,codig)


  busca_per_dis :: (Eq b, Eq c1) => ([(a, b, c2, c1)], b, c1) -> [(a, b, c2, c1)]
  busca_per_dis([],codig,peri)=[]
  busca_per_dis((cdd,cdc,nm,per):r,codig,peri)
   |cdc == codig && per == peri = (cdd,cdc,nm,per): busca_per_dis(r,codig,peri)
   |otherwise = busca_per_dis(r,codig,peri)


  busca_notas :: Eq b1 => ([(b1, b2, a, b3)], b1) -> [(a, b3)]
  busca_notas([],matri) = []
  busca_notas((mt,cd,n1,n2):r,matri)
   | mt == matri = (n1,n2): busca_notas(r,matri)
   |otherwise = busca_notas(r,matri)

  --CADASTROS


  cadastro_curso ::  IO Cursos
  cadastro_curso = do
                    putStr "Codigo do curso: "
                    cdcur <-getLine
                    putStr "Nome do curso: "
                    nome <-getLine
                    putStr "Quantidade de Periodo: "
                    per <-getLine
                    return (read cdcur, nome, read per)


  cadastro_disc :: IO Disciplinas
  cadastro_disc  = do putStr"Codigo da disciplina: "
                      cddis <-getLine
                      putStr "Codigo do curso: "
                      cdcur <-getLine
                      putStr"Nome: "
                      nome<-getLine
                      putStr"Periodo: "
                      per<-getLine
                      return(read cddis,read cdcur,nome,read per)


  cadastro_alunos :: IO Alunos
  cadastro_alunos  = do
                        putStr "Matrícula: "
                        matri <-getLine
                        putStr "Nome: "
                        nome <- getLine
                        putStr "Codigo do Curso: "
                        cdcur <-getLine
                        putStr"Periodo: "
                        per<-getLine
                        return(read matri,nome,read cdcur,read per)


  cadastro_notas ::  IO Notas
  cadastro_notas  = do
                        putStr "Matrícula: "
                        matri <-getLine
                        putStr "Codigo do Curso: "
                        cdcur <-getLine
                        putStr"Nota 1: "
                        n1<-getLine
                        putStr"Nota 2: "
                        n2<-getLine
                        if n1 == "" then do
                          putStr "Nota 1 não cadastrada\n"
                          return(read matri,read cdcur, -1 ,read n2)
                        else do
                          if n2 == "" then do
                            putStr"Nota2 não cadastrada\n"
                            return(read matri,read cdcur,read n1, -1)
                          else do
                            if n1 == "" && n2 == "" then do
                              putStr"Nenhuma nota cadastrada\n"
                              return(read matri,read cdcur,-1,-1)
                            else do
                              return(read matri,read cdcur,read n1,read n2)


  menu :: IO Int
  menu = do
    putStr"\n\n"
    putStrLn "*****MENU*****"
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
    putStrLn " 0 - Finalizar Programa"
    putStr "Digite = "
    opcao <- getLine
    putStr "\n"
    return(read opcao :: Int)



  main :: [Alunos] -> [Cursos] -> [Disciplinas] -> [Notas] -> IO ()
  main alunos cursos disciplinas notas = do
    opcao <- menu
    case opcao of
      0 -> putStr "\nObrigado por ultilizar nosso programa!\n\n"

      1 -> do aluno <- cadastro_alunos
              if null cursos then do
                putStr"Não há cursos cadastrados\n"
                main alunos cursos disciplinas notas
              else do
                if elem aluno alunos then do
                  putStr"Aluno já cadastrado!\n"
                  main alunos cursos disciplinas notas
                else do
                  main(insert aluno alunos) cursos disciplinas notas

      2 -> do curso <- cadastro_curso
              if elem curso cursos then do
                putStr"Curso já cadastrado!\n"
                main alunos cursos disciplinas notas
              else do
                main alunos (insert curso cursos) disciplinas notas

      3 -> do disc <- cadastro_disc
              if null cursos then do
                putStr"Não há cursos cadastrados\n"
                main alunos cursos disciplinas notas
              else do
                if elem disc disciplinas then do
                  putStr"Disciplina já cadastrada!\n"
                  main alunos cursos disciplinas notas
                else do
                  main alunos cursos (insert disc disciplinas) notas

      4 -> do nota <- cadastro_notas
              if null alunos || null disciplinas then do
                putStr"Impossivel\nCadastre-se já!\n"
                main alunos cursos disciplinas notas
              else do
                main alunos cursos disciplinas (insert nota notas)

      5 -> do putStr "Código do curso: "
              codiCurso <- getLine
              visualizar(buscar_alunos(alunos,(read codiCurso)))
              main alunos cursos disciplinas notas

      6 -> do putStr "Código do curso: "
              codiCurso <- getLine
              putStr "Período: "
              perd <- getLine
              visualizar(busca_per (alunos,(read codiCurso),(read perd)))
              main alunos cursos disciplinas notas

      7 -> do
              if null cursos then do
                putStr" Não há cursos a serem ofertados\n"
                main alunos cursos disciplinas notas
              else do
                putStr "Cursos ofertados: \n"
                visualizar cursos
                main alunos cursos disciplinas notas

      8 -> do putStr "Código do curso: "
              codiCurso <- getLine
              visualizar(buscar_disc(disciplinas,(read codiCurso)))
              main alunos cursos disciplinas notas

      9 -> do putStr "Código do curso: "
              codiCurso <- getLine
              putStr "Período: "
              perd <- getLine
              visualizar(busca_per_dis (disciplinas,(read codiCurso),(read perd)))
              main alunos cursos disciplinas notas

      10 -> do
              putStr "Matrícula: "
              matri <- getLine
              putStrLn"Notas:"
              visualizar (busca_notas(notas,(read matri)))
              main alunos cursos disciplinas notas

    -- if opcao < 0 || opcao > 10 then do
		-- 	putStr "\nOpção Invalida\nTente Novamente..\n\n"
		-- 	main cursos disc alunos notas

  iniciar :: IO ()
  iniciar = main [] [] [] []
