module Um where

reduzir :: (Num a) => [a] -> a
reduzir [] = 1
reduzir (x : xs) = x * reduzir xs

--1) Usando obrigatoriamente as funções generalizadoras Mapear e Reduzir, vistas
--anteriormente em sala de aula, faça uma função que receba como argumento uma lista de
--números e retorne um número que seja o produtório da metade do quadrado de cada um dos
--números da lista de entrada. (Valor 2,5)
--Ex: Lista de entrada: [1,2,3,4]
--Saída: 36 pois 0.5 * 2 * 4,5 * 8 = 36

formataLista :: (Num a) => (a -> a) -> [a] -> [a]
formataLista f [] = []
formataLista f (x : xs) = (f x) : formataLista f xs

produtorio :: (Num a, Fractional a) => [a] -> a
produtorio [] = 0
produtorio xs = reduzir (formataLista (/ 2) (formataLista (^ 2) xs))

main :: (Num a, Fractional a) => [a] -> a
main = produtorio