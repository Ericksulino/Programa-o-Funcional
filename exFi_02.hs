-- Questão 1:
-- Para a ultilização da função é preciso somente digitar "ques01" e entre parenteses
-- o numero de inicio, numero de parada, e 0 para iniciar a variavel soma com 0 sempre
-- Exemplo: ques01(3,24,0)


ques01(i,f,c)
 |i == f && mod i 3 == 0 && not (mod i 2 ==0 ) && not (mod i 5 ==0) = c
 |i == f = 0
 |i < f && mod i 3 == 0 && not (mod i 2 ==0 ) && not (mod i 5 ==0) = c + ques01(i+1,f,c+1)
 |i < f = ques01(i+1,f,c)
 |otherwise = ques01(f,i,c)

-- Questão 2:
-- Para a ultilização da função é preciso somente digitar "ques02" e entre parenteses
-- o numero de inicio, numero de parada. Exemplo: ques02(3,24)


prodiv3(i,f,p)
 | i==f && mod i 3 == 0 = p*i
 | i==f = p
 | i<f && mod i 3 == 0 = prodiv3(i+1,f,p*i)
 | i<f = prodiv3(i+1,f,p)
 |otherwise = prodiv3(f,i,p)



div100(i,f,s)
 | i==f && mod 100 i == 0 = s+i
 | i==f = s
 | i<f && mod 100 i == 0 = div100(i+1,f,s+i)
 | i<f = div100(i+1,f,s)
 |otherwise = div100(f,i,s)


ques02(i,f) = do
                let p=prodiv3(i,f,1)
                    s=div100(i,f,0)
                    res = p + s^2
                print res

-- Questão 3:
-- Para a ultilização da função é preciso somente digitar "ques03" e entre parenteses
-- os 3 numeros separados por virgula. Exemplo: ques03(12,15,16)


mdc (a,b) 
 | a < b = mdc(b,a)
 | b == 0 = a
 | otherwise = mdc (b,(mod a b))


mmc(x,y) = (x * y) `div` (mdc (x,y))


ques03(x,y,z) = mmc (x,(mmc (y,z)))

-- Questão 4:
-- Para a ultilização da função é preciso somente digitar "ques04", para a solução sem pendencia,
-- e entre parenteses os 2 numeros separados por virgula e o terceiro numero sempre é 0 
-- Exemplo: ques04(10,2,0) ou ques04(2,10,0)
-- E "ques04P" para a solução com pendencia e entre parenteses os 2 numeros separados 
-- por virgula. Exemplo: , ques04P(2,10) na ordem que preferir.

equac(n)= ((2*n)^3) + ((4*n)^2+n)

ques04(m,n,p)
 | n > m = m *(p+equac(n-1))
 | n <= m = ques04(m,n+1,equac(n-1))
 |otherwise = ques04(n,m,p)

equaP(m,n)
 | n > m = 0
 | n <= m = (equac(n)) + equaP(m,n+1)
 | otherwise = equaP(n,m)

ques04P(m,n) = m * equaP(m,n)

-- Questão 5:
-- Para a ultilização da função é preciso somente digitar "ques05" , para a solução sem pendencia,
-- e entre parenteses os 2 numeros separados por virgula.Exemplo: ques05(2,10)
-- E "ques05P" para a solução com pendencia e entre parenteses os 2 numeros separados por virgula. 
-- Exemplo: ques05(10,2) ou ques05(2,10),  na ordem que preferir


divsub :: (Ord b, Num b, Num c) => (b, b, c) -> c
divsub (x, y, r)
            | x < y = r
            | otherwise = divsub (x - y, y, r + 1)

ques05(x,y)
    | x < y = r1
    | y == 0 = - 1
    | x > y = r2
    where r1 = divsub (y, x, 0)
          r2 = divsub (x, y, 0)


divsubP(x,y)
          | x < y = 0
          |otherwise = 1+divsubP(x-y,y) 


ques05P(x,y)
 |x < y = r1P
 |y == 0 = - 1
 |x > y = r2P
 where r1P = divsubP(y,x)
       r2P = divsubP(x,y)


-- Questão 6:
-- Para a ultilização da função é preciso somente digitar "ques06" e entre parenteses, para 
-- e os 2 numeros separados por virgula. Exemplo: ques06(10,2) ou ques06(14,9) 

somx(m)
 |m == 2 = 4
 |m > 2 = m ^ 2 + somx (m-1)

somy(n)
 | n == 1 = 1
 | n > 1 = n ^ 3 + somy (n - 1)

ques06 (m, n) = somx (m) * somy (n)

