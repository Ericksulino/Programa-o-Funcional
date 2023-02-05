module Main where

--alunosNascEm anoNasc dadosAlunos = [nome| (mat, nome,(dia,mes,ano),sexo,serie) <- dadosAlunos, ano == anoNasc]
--
--
--
--imprimeAlunos [] = putStr "\n"
--imprimeAlunos (nome:r)= do
--                         putStr nome >> putStr " "
--                         imprimeAlunos r
--                        

listaAlunos = [(111,"aaaa",(1,1,2007),'F',7),(222,"bbbb",(12,06,2008),'M',6),(333,"cccc",(03,10,2007),'M',7),(444,"dddd",(14,08,2009),'M',5),(555,"eeee",(26,4,2008),'F',6),(666,"ffff",(31,7,2009),'F',5),(777,"gggg",(26,4,2008),'M',7),(888,"hhhh",(26,4,2008),'F',7)]

listaMedidas = [(111,1.60,40.0),(222,1.60,45.0),(333,1.62,48.5),(444,1.57,50.0),(555,1.55,42.0),(666,1.55,44.5),(777,1.65,50.0),(888,1.52,46.0)]

nomes160 anoNasc alt= [(mat,nome)| (mat,nome,(dia,mes,ano),sexo,serie) <-listaAlunos, (mat1,altura,peso) <- listaMedidas, ano == anoNasc, mat == mat1, altura >= alt]

imprimeAlunos2 [] = putStr "\n"
imprimeAlunos2 ((mat,nome):r)= do
                                putStr " Mat: " >> putStr (show mat)
                                putStr " Nome: " >> putStr nome
                                putStr "\n"
                                imprimeAlunos2 r

main = imprimeAlunos2(nomes160 2008 1.55)                         