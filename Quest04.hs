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


-- *** Parte 1 - segurança

--  verificações 
verificaCurso :: [Curso] -> Integer -> [Curso] 
verificaCurso lista codigo = [(cod_curso, nm_curso, qtd_perido) |
							(cod_curso, nm_curso, qtd_perido) <- lista, cod_curso == codigo]

verificaDisc :: [Disciplina] -> Integer -> [Disciplina] 
verificaDisc lista codigo = [(cod_disc, cod_curso, nm_disc, perido) |
							(cod_disc, cod_curso, nm_disc, perido) <- lista, cod_disc == codigo]


verificaAluno :: [Aluno] -> Integer -> [Aluno] 
verificaAluno lista codigo  = [(matricula, nm_aluno, (cod_curso, nm_curso, qtd_perido), perido) |
							(matricula, nm_aluno, 
							(cod_curso, nm_curso, qtd_perido), perido) <- lista, matricula == codigo]


-- Buscas
buscarAlunos :: [Aluno] -> Integer ->Int -> [Aluno] 
buscarAlunos lista codigo per
	
	|per == 0 = [(matricula, nm_aluno, (cod_curso, nm_curso, qtd_perido), perido) | (matricula, nm_aluno, 
				 (cod_curso, nm_curso, qtd_perido), perido) <- lista, cod_curso == codigo]

	|otherwise = [(matricula, nm_aluno, (cod_curso, nm_curso, qtd_perido), perido) | 
				  (matricula, nm_aluno, (cod_curso, nm_curso, qtd_perido), perido) <- lista, 
				  cod_curso == codigo, per == perido]

buscarDisc :: [Disciplina] -> Integer -> Int-> [Disciplina] 
buscarDisc lista codigo per 
	| per == 0 = [(cod_disc, cod_curso, nm_disc, perido) |
				  (cod_disc, cod_curso, nm_disc, perido) <- lista, cod_disc == codigo]
	
	| otherwise = [(cod_disc, cod_curso, nm_disc, perido) |
				   (cod_disc, cod_curso, nm_disc, perido) <- lista, cod_disc == codigo, 
				   per == perido]

buscarNotas :: [Notas] -> Integer -> [Notas]
buscarNotas lista mat = [(matricula, cod_disc, n1, n2) | 
					 (matricula, cod_disc, n1, n2) <- lista, matricula == mat]

imprimir [] = putStr "\n" 
imprimir (c: r) = do 
				print (c)
				imprimir r


-- *** Parte 2 - Cadastros

-- Cadastros
msg :: IO()
msg = putStr "\nDados Cadastrados com Sucesso!\n\n"

cadastrarCurso ::  IO Curso
cadastrarCurso = do 

	putStr "Codigo do curso: "
	cod_curso <-getLine 
	putStr "Nome do curso: "
	nm_curso <-getLine
	putStr "Quantidade de Periodo: "
	qtd_perido <-getLine
	msg
	return (read cod_curso, nm_curso, read qtd_perido)

cadastrarDisc :: [Curso] -> IO Disciplina
cadastrarDisc lista = do
	
	putStr "Codigo da disciplina: "
	cod_disc <-getLine 
	putStr "Codigo do curso: "
	cod_curso <-getLine

	if null(verificaCurso lista (read cod_curso)) then do
		putStr "\nCurso não Cadastrado\nTente Novamente..\n\n"
		cadastrarDisc lista

	else do
	
		putStr "Nome da disciplina: "
		nm_disc <-getLine
		putStr "Periodo: "
		perido <-getLine

		msg

		return (read cod_disc, read cod_curso, nm_disc, read perido)


cadastrarAluno :: [Curso] -> IO Aluno
cadastrarAluno lista = do
	
	putStr "Matrícula: "
	matricula <-getLine
	putStr "Nome: "
	nm_aluno <- getLine
	putStr "Codigo do Curso: "
	cod_curso <-getLine

	let curso = verificaCurso lista (read cod_curso)
	
	if null curso then do
		putStr "\nCurso não Cadastrado\nTente Novamente..\n\n"
		cadastrarAluno lista

	else do
		putStr "Período: "
		perido <-getLine

		msg

		return (read matricula, nm_aluno, (head curso), read perido)


cadastrarNotas :: [Aluno] -> [Disciplina] -> IO Notas
cadastrarNotas l1 l2 = do

	putStr "Matrícula: "
	matricula <-getLine

	putStr "Codigo da disciplina: "
	cod_disc <- getLine

	if null(verificaAluno l1 (read matricula)) then do
		putStr "\nAluno não Cadastrado\nTente Novamente..\n\n"
		cadastrarNotas l1 l2

	else if null(verificaDisc l2 (read cod_disc)) then do
		putStr "\nDisciplina não Cadastrada\nTente Novamente..\n\n"
		cadastrarNotas l1 l2

	else do

		putStr "Notas\n"
		putStr "1 - Para Informar uma nota\n"
		putStr "2 - Para Informar duas notas\n"
		putStr "3 - Não Informar Notas\n>> "
		op <- getLine
		
		if op == "1" then do
			putStr "Nota 1: "
			n1 <-getLine

			return (read matricula, read cod_disc, (read n1 :: Float), -1)

		else if op == "2" then do
			putStr "Nota 1: "
			n1 <-getLine
			putStr "Nota 2: "
			n2 <-getLine
			return (read matricula, read cod_disc, (read n1:: Float), (read n2 :: Float))

		else
			return (read matricula, read cod_disc, -1, -1)



-- *** Parte 3 - Interação com usuario 
menu :: IO String
menu = do
        putStr "\n1 - Cadastrar Cursos\n"
        putStr "2 - Cadastrar Disciplinas\n"
        putStr "3 - Cadastrar Alunos\n"
        putStr "4 - Cadastrar Notas\n"
        putStr "5 - Mostrar Cursos\n"
        putStr "6 - Mostrar alunos de um cursos\n"
        putStr "7 - Mostrar alunos por perido\n"
        putStr "8 - Disciplinas de um Curso\n"
        putStr "9 - Disciplinas de um periodo do curso\n"
        putStr "10 - Vizualizar notas de Aluno\n"
        putStr "11 - Sair\n>> "
        op <- getLine
        return op 

main :: [Curso] -> [Disciplina] -> [Aluno] -> [Notas] -> IO ()
main cursos disc alunos notas = do

	op <-menu

	if op /= "11" then do

		if op == "1" then do
			curso <- cadastrarCurso
			main (insert curso cursos) disc alunos notas

		else if op == "2" then do
				if null cursos then do
					putStr "\nNão existem cursos cadastrados\n\n"
					main cursos disc alunos notas
				else do
					dis <-cadastrarDisc cursos
					main cursos (insert dis disc) alunos notas

		else if op == "3" then do
			if null cursos then do
					putStr "\nNão existem cursos cadastrados\n\n"
					main cursos disc alunos notas
				else do
					al <-cadastrarAluno cursos
					main cursos disc (insert al alunos) notas


		else if op == "4" then do
			if null disc || null alunos then do
					putStr "\nImpossivel realizar processo\n\n"
					main cursos disc alunos notas
				else do
					nota <-cadastrarNotas alunos disc
					main cursos disc alunos (insert nota notas)

		else if op == "5" then do
				if null cursos then do
					putStr "\nNão existem cursos cadastrados\n\n"
					main cursos disc alunos notas
				else do
					putStr "\nCursos Disponiveis\n"
					imprimir cursos
					main cursos disc alunos notas
		
		else if op == "6" then do
			putStr "Informe o codigo do curso: "
			cod_curso <- getLine
			
			imprimir (buscarAlunos alunos (read cod_curso) 0)
			main cursos disc alunos notas


		else if op == "7" then do
			putStr "Informe o codigo do curso: "
			cod_curso <- getLine

			putStr "Informe o Periodo: "
			periodo <- getLine
			
			imprimir (buscarAlunos alunos (read cod_curso) (read periodo))
			main cursos disc alunos notas

		else if op == "8" then do
			putStr "Informe o codigo do curso: "
			cod_curso <- getLine

			
			imprimir (buscarDisc disc (read cod_curso) 0)
			main cursos disc alunos notas

		else if op == "9" then do
			putStr "Informe o codigo do curso: "
			cod_curso <- getLine

			putStr "Informe o Periodo: "
			periodo <- getLine
			
			imprimir (buscarDisc disc (read cod_curso) (read periodo))
			main cursos disc alunos notas								

		else if op == "10" then do
			putStr "Matrícula: "
			matricula <- getLine

			imprimir (buscarNotas notas (read matricula))

		else do
			putStr "\nOpção Invalida\nTente Novamente..\n\n"
			main cursos disc alunos notas					

	else
		putStr "\nObrigado por ultilizar nosso programa!\n\n"


masterMain = main [] [] [] []