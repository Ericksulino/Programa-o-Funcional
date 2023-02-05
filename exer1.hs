module Main where

  import Data.Char
  import Data.List

  lista = ["1aca","!va","Pera","rock"]

  -- letra a
  countChars :: [String] -> [Int]
  countChars = fmap (length . group . sort)

  -- letra b
  vogal(c)
    | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = True
    | otherwise = False

  caract(c)
    | c == '!' || c == '@' || c == '#' || c == '$' || c == '%' || c == '*' = True
    | otherwise = False

  consoant(c)
    | vogal(c) == False && caract(c) == False && isDigit(c) == False = True
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

  contV [] = 0
  contV (c:r)
    | vogal(toLower(c)) = 1 + contV (r)
    | otherwise =contV r

  letraC([],maior) = []
  letraC(c:r,maior)
    | contV c > maior = c: letraC(r,contV c)
    | otherwise = letraC(r,maior)

  countVog(lista) = last(letraC(lista,0))

  main :: IO ()
  main = do
    print (countChars lista)
    print(countCarac(lista))
    print(countVog(lista))
