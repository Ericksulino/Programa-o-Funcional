-- Feita com sucesso
module PrimeiraQuest where

fatorial(n)
    | n == 0 = 1
    | n > 0 = fatorial(n-1) * n

proCP(n)
    | n == 5 = 300
    | n > 5 = (f1*5) / (f2*2) * proCP(n - 1)
     where f1 = fatorial(n)
           f2 = fatorial(n-5)


proSP(n,aux)
    | n == 5 = aux*prod
    | n > 5 = proSP(n-1,aux*prod)
    where prod = (fatorial(n)*5) / (fatorial(n-5)*2)


chamaFuncao(calculo,num)
    | calculo == "p" = print(proCP(num))
    | calculo == "sp" = print(proSP(num,1))

main :: IO ()
main = do
    putStr "Digite\np- Com pendencia\nsp- Sem pendencia\n"
    calculo <- getLine
    putStr "Digite um numero para o calculo do produtório\n"
    num <- getLine
    let produtorio = chamaFuncao(calculo,(read num))
    putStr"O resultado eh: "
    produtorio
