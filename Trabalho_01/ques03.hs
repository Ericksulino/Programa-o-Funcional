-- função mdc recebe com parametros os 2 primeiros numeros, a e b, se a for menor que b,
-- é chamda a função novamente invertendo a ordem dos mesmos se b for igual a 0
-- é retornado o a, se não entrar nas anteriores e chamada a função novamente
-- dessa vez com b e o resto da divisão de a e b como parametros
module TerceiraQuest where
mdc::Int->Int->Int
mdc a b
 | a < b = mdc b a
 | b == 0 = a
 | otherwise = mdc b (mod a b)

-- função mdc3 recebe como paremetros os 3 numeros, a,b e c, e chama a função mdc com a
-- como o primeiro parametro e a propria função mdc com os paremetros b e c como o segundo
-- parametro, assim: 

mdc3::Int->Int->Int->Int
mdc3 a b c = mdc a (mdc b c)

main :: IO ()
main = do
         putStr"digite o primeiro numero: "
         n1<-getLine
         putStr"digite o segundo numero: "
         n2<-getLine
         putStr"digite o terceiro numero: "
         n3<-getLine
         let res = mdc3 (read n1) (read n2) (read n3)
         putStr"O maximo divisor comum eh: "
         print res


