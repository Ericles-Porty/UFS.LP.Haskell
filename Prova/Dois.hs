module Dois where

reduzir ::  [Int] -> Int
reduzir [] = 1
reduzir (x : xs) = x * reduzir xs

--2) Usando a função reduzir, vista em sala, e List Comprehension faça uma (ou mais de uma)
--função (ou funções) que leia(m) uma lista de inteiros e retorne uma lista com o fatorial dos
--números ímpares que fazem parte da lista. (Valor 2,0)

fato :: Int -> [Int]
fato 1 = []
fato x = x: fato (x-1)

criaLista :: [Int] -> [Int]
criaLista [] = []
criaLista xs = [reduzir (fato x) | x <- xs,mod x 2 == 1]

main :: [Int] -> [Int]
main = criaLista