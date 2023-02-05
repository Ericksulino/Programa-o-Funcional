module QuintaQuest where

import System.Random (randomRIO)

mesa(n,i,c)
 |c==n = i
 |c<n && i ==2 = mesa(n,i-2,c+1)
 |c<n = mesa(n,i+1,c+1)

bia(n,n2)
 | n==mesa(n2,0,0) && n == 2 = 0
 | n==mesa(n2,0,0) && n <2 = n+1
 | n/=mesa(n2,0,0)= mesa(n2,0,0)


carol(n1,n2)
 | (n1==1 && n2==2) || (n1==2 && n2==1) = 0
 | (n1==0 && n2==2) || (n1==2 && n2==0) = 1
 | (n1==0 && n2==1) || (n1==1 && n2==0) = 2

main = do
        na <- randomRIO(1,100 :: Int)
        putStr"Número sorteado de Ana: "
        print na
        putStr"Número sorteado de Beatriz: "
        nb <- randomRIO(1,100 :: Int)
        print nb
        let nA = mesa(na,0,0)
            nB = bia(nA,nb)
            nC = carol(nA,nB)
        putStr"Ana vai sentar na cadeira de número: "
        print nA
        putStr"Beatriz vai sentar na cadeira de número: "
        print nB
        putStr"Carolina vai sentar na cadeira de número: "
        print nC        
