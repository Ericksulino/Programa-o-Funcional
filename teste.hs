multipli(x,y) = x*y

somawh(n1,n2) = do
                  print res
                  where res = n1+n2

somale(n1,n2) = do
                  let res = n1+n2
                  print res
--Fatoriais
fat :: Int -> Int
fat n
 | n == 0 = 1
 | otherwise = n * fat(n-1)

fatp :: (Eq b, Num b) => (b, b) -> b
fatp(n,r)
 | n == 0 = r
 | otherwise = fatp((n-1),r*n)

fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial 1 = 1
fatorial n = fatorial' n 1
  where
    fatorial' 1 r = r
    fatorial' n r = fatorial' (n-1) (n*r)
-- fibonaci
fibon :: Int -> Int
fibon n
  | n == 1 = 1
  | n == 2 = 1
  | otherwise = fibon (n-1) + fibon (n-2)

mesa(n,i,c)
 | c==n = i
 |c<n && i ==2 = mesa(n,i-2,c+1)
 |c<n = mesa(n,i+1,c+1)

l1 = [1,2,3,4,5,6,7,10,12]
l2 = [4,9,10,15,26,30]

--Os pares de uma lista
lispa [] = []
lispa(c:r)
 |even c = c: lispa r
 |otherwise = lispa r

ivlis [] = []
ivlis (c:r) = ivlis r ++ [c]


--Mostra se é palindromo ou não
isPali xs = xs == (reverse xs)
isPalindrome' []  = True
isPalindrome' [_] = True
isPalindrome' xs  = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)

reverso a = reverse a


ques3C [] = 0
ques3C (c:r)
 |c==' ' = 1+ques3C r
 |otherwise = ques3C r


-- ques06 = do putStr"(a) quantas pessoas na lista são do sexo feminino: "
--            que6A
--            putStr"/n"
--            putStr"(b) quantas pessoas da lista possuem mais de 40 anos: "
--            que6B
--            putStr"/n"
--            putStr"(c) uma lista contendo o nome das mulheres com mais de 40 anos: "
--            que6C
--            putStr"/n"
--            putStr"(d) a média da idade dos homens: "
--            que6D
--            putStr"/n"

ques05 ([],[]) = []
ques05 (l1,[]) = []
ques05 ([],l2) = []
ques05(c1:r1,c2:r2)
 |head c1 == head c2 = ques05 (r1,r2)
 |head c1 < head c2 = ques05 (r1,c2:r2)
 |head c1 > head c2 = ques05 (c1:r1,r2)


--mis i f
--  | i==f && odd i = i^3
--  | i==f = i^2
--  | i<f && odd i== i^3* mis(i+1) f
--  | otherwise = i^2* mis (i+1) f

--mmis = print(mis 20 45 1)

equa :: (Num a, Ord a) => a -> a -> a -> a -> a -> a
equa x y m s1 s2
 |x==m && y == x-1 = s1 + x* s2
 | x==m && y<x = s2 + y* s1
 |x<m && y==x = equa (x+1) y m (s1+(x*(y+s2))) s2
 | x < m || y<x = equa x (y+1) m (x*s1) (y+s2)

mequa = print(equa 3 2 5 0 0 )

--diz o maior elemento de uma lista
maior ([],m) = m
maior(c:r,m)
 | c>m = maior(r,c)
 |otherwise = maior(r,m)

-- eleva os elementos de uma lista
elev :: (Integral b, Num a) => ([a], b) -> [a]
elev([],m) = []
elev(c:r,m) = x: elev(r,m)
            where x = c^m

--os multiplos de uma lista
multi([],m) = []
multi(c:r,m)
 | mod c m == 0 = c: multi(r,m)
 |otherwise = multi(r,m)

--uni duas listas de forma ordenada
lisuni :: Ord a => ([a], [a]) -> [a]
lisuni ([],[]) =[]
lisuni (l1,[]) = l1
lisuni ([],l2 )= l2
lisuni (c1:r1,c2:r2)
 |c1 == c2 = c1: lisuni(r1,r2)
 |c1 < c2 = c1: lisuni(r1,c2:r2)
 |otherwise = c2: lisuni(c1:r1,r2)

nesimo a n= a!!n


maio :: (Foldable t, Ord a) => t a -> a
maio a = maximum a

menor a = minimum a
case x of
        0 -> 18
        1 -> 15
        2 -> 12
        3_-> 12