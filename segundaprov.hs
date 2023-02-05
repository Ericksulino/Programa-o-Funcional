
import Data.Char

lisuni :: Ord a => ([a], [a]) -> [a]
lisuni ([],[]) =[]
lisuni (l1,[]) = l1
lisuni ([],l2 )= l2
lisuni (c1:r1,c2:r2)
 |c1 == c2 = c1: lisuni(r1,r2)
 |c1 < c2  = c1: lisuni(r1,c2:r2)
 |otherwise = c2: lisuni(c1:r1,r2)

somaimp :: Integral a => [a] -> [a] -> a
somaimp x y = sx+sy
              where sx = sum(filter odd x)
                    sy = sum(filter odd y)

pares a = even a  


inter :: ([a], [a]) -> [a]
inter ([],[]) =[]
inter (l1,[]) = l1
inter ([],l2 )= l2
inter (c1:r1,c2:r2) = c1: c2: inter(r1,r2)


cubomaiores x = map (^3) (filter even (filter(>10) (filter(<50) x)))




porce x = map(*10) ((map(/100) m100))
        where imp = filter odd x
              m100 = filter (>100) imp

prodquapar x = product (map(^2)(filter even x))

duaslistas x num = [take num x] ++ [drop num x]

vogais(c)
 |c == 'a'|| c == 'e'|| c == 'i'|| c == 'o'||c == 'u' = True
 |otherwise = False

prime [] = []
prime((p,re):r) = p

letra l = [isUpper(resultado)| resultados <- l, vogais(head(resultado))]

--funct lista = [x | x <- [lista], length(x) > 5, not(vogais(x))]