
---- lista é vazia
---- lista e < cabeça lista
---- lista e == cabeça da lista
---- lista e > cabeça da lista

e= [8,10,12,14,15,16,18]     

insereOrd :: Ord t => t -> [t] -> [t]
insereOrd e [] = [e]
insereOrd e (c:r)
  | e < c = e:c:r 
  | e == c = c:r
  | otherwise = c : insereOrd e r








