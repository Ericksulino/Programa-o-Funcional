import System.IO
    ( hClose, hFlush, openFile, hPrint, IOMode(WriteMode) ) 

lista :: [([Char], [Char], Integer)]
lista = [("Flamengo","rj",1234),("Vasco","rj",1234)]

escrever :: IO ()
escrever = do arq<- openFile "teste.txt" WriteMode
              hPrint arq lista
              putStrLn"Escrito com sucesso!"
              hFlush arq
              hClose arq 

ler :: IO ()
ler = do conteudo<-readFile"teste.txt"
         putStrLn conteudo

anexar :: IO ()
anexar = do appendFile"teste.txt" "\nHaskell eh legal"
            putStrLn"anexado com sucesso!"



