module QuartaQuest where
divis :: Integral a => (a, a, a) -> a
divis(i,n,s)
 | i==n && mod n i == 0 = s
 | i==n = s
 | i<n && mod n i == 0 = divis(i+1,n,s+i)
 | i<n = divis(i+1,n,s)


perf :: Integral a => a -> IO ()
perf n
    | n > 0 && divis (1, n, 0) == n = do putStrLn"Eh perfeito!"
                                         main
    | n > 0 && divis (1, n, 0) > n = do putStrLn"Eh abundante!"
                                        main
    | n > 0 && divis (1, n, 0) < n = do putStrLn"Eh deficiente!"
                                        main
    | n==0 = putStrLn"Finalizado com sucesso!"
    | n<0 = do putStrLn"Entrada incorreta\n Por favor, digite novamente"
               main

main :: IO ()
main = do
        putStr"Digite um numero ou 0 para sair: "
        nro <- getLine
        perf(read nro)
