module Main where


impIntervalo(i,f)
 | i == f && mod i 2 == 0 = print i
 | i == f = putStr ""
 | i < f  && mod i 2 == 0 = do 
                             print i
                             impIntervalo(i+1,f) 
 | i < f = impIntervalo(i+1,f)                           
 | otherwise = impIntervalo(f,i)



impMulti23(i,f)
 | i == f && mod i 2 == 0 || mod i 3 == 0 = print i
 | i == f = putStr ""
 | i < f  && mod i 2 == 0 || mod i 3 == 0 = do 
                                               print i
                                              impIntervalo(i+1,f) 
 | i < f = impIntervalo(i+1,f)                           
 | otherwise = impIntervalo(f,i)
              