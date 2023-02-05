module Quest04 where

import Data.List(insert)

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

cadastrarCurso ::  IO Curso
cadastrarCurso = do 

	putStr "Codigo do curso: "
	cdcur <-getLine 
	putStr "Nome do curso: "
	nome <-getLine
	putStr "Quantidade de Periodo: "
	per <-getLine
	return (read cdcur, nome, read per)

cadastrarDisc :: [Curso] -> IO Disciplina
cadastrarDisc lista = do
	
                        putStr "Codigo da disciplina: "
                        cddis <-getLine 
                        putStr "Codigo do curso: "
                        cdcur <-getLine
                        putStr"Nome: "
                        nome<-getLine
                        putStr"Periodo: "
                        per<-getLine
                        return(read cddis,read cdcur,nome,read per)