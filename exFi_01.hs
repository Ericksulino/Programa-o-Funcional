-- as funções são nomeadas por "ques" e o numero da questão correspondente. Ex ques01 para a questão
-- de numero 01 e assim sucessivamente. 

-- QUESTÃO 1:
-- Digite primeiro se vc quer quadrado, retangulo ou triangulo: (1,2 ou 3), depois a base seguida 
-- da altura. Exemplo para calcular a area de um triangulo: ques01(3,5,10), onde o 3 quer dizer 
-- o triangulo, o 5 a sua base e o 10 a sua altura.
leC1 a b
 | maior(a,0)>maior(b,0) = maior(a,0)
 |otherwise = maior(b,0)
ques01(p,b,a)
 | p == 1 || p == 2 = print arQR
 | p == 3 = print arT
    where arQR = b*a
          arT = b*a/2

--QUESTÃO 2:
-- Digite os lados do triangulo na ordem que preferir Ex: (3,8,2) ou (8,3,2) e assim sucessivamente
-- mas sempre coloque entre parenteses e com virgula entre os numeros 

ques02(a,b,c) = a+b > c && b+c > a && a+c > b

--QUESTÃO 3:
-- Para a resolucao da questão 3 foram feitas a funcao principal e uma auxiliar, mas para 
-- ultilizar elas so eh necessario digitar ques03 e entre parenteses digitar os 3 numeros
-- desejados. Exemplo: ques03(5,3,2).

maior(m,n)
 |n > m = 1
 |otherwise = 0

ques03(n1,n2,n3) = do
                     let med = (n1+n2+n3)/3
                         c1 = maior(med,n1)
                         c2 = maior(med,n2)
                         c3 = maior(med,n3)
                         cont = c1+c2+c3
                     print cont

--QUESTÃO 4:
-- primeiro digite o numero que deseja converter, segundo a medida em que ele está, depois a 
--medida que deseja converter. Exemplo: ques04(15,2,1). Considere que: 1=Km, 2=m,3=cm,4=mm

ques04(n,md1,md2)
 | md1 == 1 && md2 == 2 = n*1000
 | md1 == 1 && md2 == 3 = n*100000
 | md1 == 1 && md2 == 4 = n*1000000
 | md1 == 2 && md2 == 1 = n/1000
 | md1 == 2 && md2 == 3 = n*100
 | md1 == 2 && md2 == 4 = n*1000
 | md1 == 3 && md2 == 1 = n*100000
 | md1 == 3 && md2 == 2 = n/100
 | md1 == 3 && md2 == 4 = n*10
 | md1 == 4 && md2 == 1 = n/1000000
 | md1 == 4 && md2 == 2 = n/1000
 | md1 == 4 && md2 == 3 = n/10

--QUESTÃO 5:
-- Digite os dois numeros entre parenteses separados por virgula

ques05(x,y)
 | x > y = x + (x/y)^2
 | x < y = y * (y-x)
 | x == y = y^3 +(x+y)^2

-- QUESTÃO 6:
-- Para a resolucao da questão 6 foram feitas a funcao principal e duas auxiliares, mas para 
-- ultilizar elas so eh necessario digitar ques06 e entre parenteses o salario bruto mensal e o
-- Imposto pago mensal respectivamente, por Exemplo: ques06(3000,1000).

impDev(bc)
 | bc <= 22000 = 0
 | bc > 22000 && bc <33000 = bc*7.5/100
 | bc > 33000 && bc <45000 = bc*15/100
 | bc > 45000 && bc <55000 = bc*22.5/100
 | bc>55000 = bc*27.5/100

res(ip,ipd) = ip*12 - ipd

ques06(sb,ir) = do
                  putStr"Imposto devido eh: "
                  print ipd
                  putStr"Restituicao eh: "
                  print r
                  where sba = sb*12
                        bc = sba - sba*20/100
                        ipd = impDev(bc)
                        r = res(ir,ipd)

--QUESTÃO 7:
-- Para executar a funcao "ques07", é preciso somente digitar "ques07", pois a mesma
-- não tem parametros de entrada.

ques07 = do
         let cf = 100000
             cs = 2000
             ing = 24
             cap = 500
             lpf = cap*ing-cs
             ns = cf/lpf
             ni = ns * cap
         putStr"Numero de seçoes: "
         print ns
         putStr "Numero de ingressos: "
         print ni




