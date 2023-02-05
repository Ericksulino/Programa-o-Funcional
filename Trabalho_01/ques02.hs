module SegundaQuest where

  fatora(numero,divi,i)
    | numero == 1 = putStr "\n"
    | mod numero divi == 0 = do
       putStr(show divi) >> putStr "-" >> putStr(show i) >> putStr "\n"
       fatora(div numero divi,divi,i+1)
    | mod numero divi /= 0 = do
      let cont = 1
      fatora(numero,divi+1,cont)

  main = do 
    putStr"Digite o primeiro número: "
    num1<-getLine
    putStr"Digite o segundo número: "
    num2<-getLine
    putStr"Digite o terceiro número: "
    num3<-getLine
    putStrLn"Número-Vezes que apareceu:"
    fatora((read num1),2,1)
    fatora((read num2),2,1)
    fatora((read num3),2,1)
