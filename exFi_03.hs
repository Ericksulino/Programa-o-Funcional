module Lista_3 where

import Data.Char ( isUpper, toLower )
--Questão 1
-- Para a resolução da mesma foram feitas duas funções auxiliares e uma principal.
-- Para execução dela é somente necessario digitar "ques01" seguido pelas duas listas
-- separadas uma da outra por espaço. Exemplo: ques01 [1,2,3,6] [2,4,5,8,9]s
lispa :: Integral a => [a] -> [a]
lispa [] = []
lispa(c:r)
 |even c = c: lispa r
 |otherwise = lispa r
 
lisuni :: Ord a => ([a], [a]) -> [a]
lisuni ([],[]) =[]
lisuni (l1,[]) = l1
lisuni ([],l2 )= l2
lisuni (c1:r1,c2:r2)
 |c1 == c2 = c1: lisuni(r1,r2)
 |c1 < c2 = c1: lisuni(r1,c2:r2)
 |otherwise = c2: lisuni(c1:r1,r2)

ques01 :: Integral a => [a] -> [a] -> [a]
ques01 l1 l2 = lisuni(lispa l1 ,lispa l2)

--Questão 2
--Para a execução da função é somente necessário digitar "ques02" e em seguida a string que deseja transformar
-- em inteiro entre aspas duplas. Exemplo: ques02 "12345678"     
ques02 str = read str :: Int

--Questão 3
-- Para execução das funções é somente necessário digitar "ques2" seguido da letra referente e a entrada
-- da mesma. Ex: ques3A "ovo" , ques3B "funcional", etc.

ques3A str = str == reverse str

vogais(c)
 |c == 'a'|| c == 'e'|| c == 'i'|| c == 'o'||c == 'u' = True
 |otherwise = False
ques3B [] = []
ques3B (c:r)
 |vogais(toLower(c)) = c: "*"++ ques3B (r)
 |otherwise =c: ques3B r

ques3C [] = 1
ques3C (c:r)
 |c==' ' = 1+ques3C r
 |otherwise = ques3C r
-- Questão 4
-- Para execução das funções é somente necessário digitar "ques4" seguido da letra referente
-- e depois o seu parametro Ex: ques4A(["Programacao","Sistemas",0), ques4B(["Programacao","UFPI",6), etc. 
-- Ou simplismente digite "ques06" para executar todas de uma só vez.
ques4A([],maior) = []
ques4A(c:r,maior)
 | length c> maior = c: ques4A(r,length c)
 |otherwise = ques4A(r,maior)

ques4B([],m) = []
ques4B(c:r,m)
 | length c<m = c: ques4B(r,m)
 |otherwise = ques4B(r,m)

ques4C([]) = 0
ques4C(c:r)
 | isUpper(head(c)) = 1+ques4C(r)
 |otherwise = ques4C(r)
ques4D([]) = []
ques4D(c:r)
 | isUpper(head(c)) = c:ques4D(r)
 |otherwise = ques4D(r)

listr = ["funcional","Sistemas","de","informacao","campus","Picos","UFPI","SHNB","Programacao"]
ques04 = do putStr"(a) a maior string da lista:"
            print(ques4A(listr,0))
            putStr"(b) uma lista onde todas as strings são menores do que 6: "
            print(ques4B(listr,6))
            putStr"(c) a quantidade de strings iniciam com letra maiúscula: "
            print(ques4C(listr))
            putStr"(d) uma lista contendo somente as strings que iniciam com letra maiúscula: "
            print(ques4D(listr))
-- Questão 5
--Para a execução da função é somente necessário digitar "ques05" e em seguida as duas strings
--Exemplo: ques05 "programação" "funcional"
ques05 a b = a++b 

-- Questão 6
-- Para execução das funções é somente necessário digitar "ques6" seguido da letra referente
-- Ex: ques6A, ques6B, etc. Ou simplismente digite "ques06" para executar todas de uma só vez.

lis = [(1256,"João ",'M',20),(1562,"Antônio ",'M',43),(1652,"Jose ",'M',45),(5261,"Isabel ",'F',60),
       (6125,"Joana ",'F',54),(6251,"Ana ",'F',40),(6512,"Pedro ",'M',50),(6521,"Maria ",'F',20)]

qtdpes ([],p) = 0
qtdpes((c,n,s,id):r,p)
 | s == p = 1 + qtdpes(r,p)
 | otherwise = qtdpes(r,p)

que6A = qtdpes(lis,'F')

qtd40 ([]) = 0
qtd40((c,n,s,id):r)
 | id > 40 = 1 + qtd40(r)
 | otherwise = qtd40(r)

que6B = qtd40(lis)

qtd40F ([],p) = []
qtd40F((c,n,s,id):r,p)
 | id > 40 && s ==p = n: qtd40F(r,p)
 | otherwise = qtd40F(r,p)

que6C = qtd40F(lis,'F')

somId ([]) = 0
somId((c,n,s,id):r)
 | s =='M' = id + somId(r)
 | otherwise = somId(r)

que6D = somId(lis)/qtdpes(lis,'M')

ques06 = do putStr"(a) quantas pessoas na lista são do sexo feminino: "
            print(que6A)
            putStr"(b) quantas pessoas da lista possuem mais de 40 anos: "
            print(que6B)
            putStr"(c) uma lista contendo o nome das mulheres com mais de 40 anos: "
            print(que6C)
            putStr"(d) a média da idade dos homens: "
            print(que6D)