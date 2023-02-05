module Primeira where

  import Data.Char ( isDigit, toLower )
  import Data.List ( sort, group )

  lista :: [[Char]]
  lista = ["maca","uva","pera","banana","abacaxi","aeiou","!salada"]

  -- letra a
  countChars :: [String] -> [Int]
  countChars = fmap (length . group . sort)

  -- letra b
  vogal :: Char -> Bool
  vogal(c)
    | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = True
    | otherwise = False

  caract :: Char -> Bool
  caract(c)
    | c == '!' || c == '@' || c == '#' || c == '$' || c == '%' || c == '*' = True
    | otherwise = False

  consoant :: Char -> Bool
  consoant(c)
    | vogal(c) == False && caract(c) == False = True
    | otherwise = False

  countCarac :: [String] -> [String]
  countCarac([]) = []
  countCarac(c:r)
    | vogal(head(c)) = "vogal" : countCarac r
    | caract(head(c)) = "caractere" : countCarac r
    | consoant(head(c)) = "consoante" : countCarac r
    | isDigit(head(c)) = "digito" : countCarac r
    | otherwise = "outro" : countCarac r

  -- letra c

  contV :: Num p => [Char] -> p
  contV [] = 0
  contV (c:r)
    | vogal(toLower(c)) = 1 + contV (r)
    | otherwise =contV r

  letraC :: (Ord b, Num b) => ([[Char]], b) -> [[Char]]
  letraC([],maior) = []
  letraC(c:r,maior)
    | contV c> maior = c: letraC(r,contV c)
    | otherwise = letraC(r,maior)

  countVog :: [[Char]] -> [Char]
  countVog(lista) = last(letraC(lista,0))

  main :: IO ()
  main = do
    print (countChars lista)
    print(countCarac(lista))
    print(countVog(lista))
