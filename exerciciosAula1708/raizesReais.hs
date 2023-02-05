module Main where

raizes_reais1 (a,b,c)
 | delta < 0 = error "Raizes Imaginarias"
 | delta >= 0 = (r1,r2)
    where d = 2*a
          delta = b^2-4*a*c
          r1 = (-b+sqrt(delta))/d
          r2 = (-b-sqrt(delta))/d


raizes_reais2 (a,b,c)
 | b^2-4*a*c < 0 = error "Raizes Imaginarias"
 | b^2-4*a*c >= 0 = let
                      d = 2*a
                      delta = b^2-4*a*c
                      r1 = (-b+sqrt(delta))/d
                      r2 = (-b-sqrt(delta))/d
                    in (r1,r2)

calculaDelta(a,b,c) = bˆ2 - 4*a*c

calculaRaizes(a,b,delta) = let
                            d = 2*a
                            r1 = (-b+sqrt(delta))/d
                            r2 = (-b-sqrt(delta))/d
                           in (r1,r2)
                             

raizes_reais3(a,b,c) = do
                        if (delta >= 0)
                         then print(calculaRaizes(a,b,delta))
                         else putStr "Não existe Raízes Reais."
                        where
                         delta = calculaDelta(a,b,c)
--main :: IO()
main = do
        putStrLn "Digite A: "
        a <- getLine
        putStrLn "Digite B: "
        b <- getLine
        putStrLn "Digite C: "
        c <- getLine
        raizes_reais3(read a,read b, read c) 

