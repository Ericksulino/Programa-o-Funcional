main :: IO ()
main = start([])

start :: [String] -> IO ()
start(l) = creatLista(l) 

creatLista(l) = do
                putStrLn ":: OPCOES ::"
                putStrLn ("L=" ++ (show l))
                putStrLn " 1 - Adicionar string na lista"
                putStrLn " 2 - Executa tarefas "
                putStrLn " 3 - Limpar lista"
                putStrLn " 0 - Finalizar"
                putStr ":: Opcao="
                opcao <- getLine
                putStr "\n"
                case opcao of
                            "0" -> return()
                            "1" -> do
                                    putStrLn "OBS: Digite sem acentos e cedilha, bem como, sem caracteres!!"
                                    putStr "Digite uma string: "
                                    string <- getLine
                                    creatLista(string:l)
                            "2" -> (verNull(l))
                            "3" -> do
                                            putStrLn ":: LISTA ESVAZIADA COM SUCESSO! "
                                            creatLista([])


verNull :: [String] -> IO ()
verNull(l)
            | l == ([]) = do
                            putStrLn "LISTA VAZIA!!"
                            creatLista([])
            | otherwise = menu(l)