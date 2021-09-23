module Listas where

--Crie uma função que receba uma lista de inteiros e retorne quantos elementos há na lista.
quantosElementos :: [Int] -> Int
quantosElementos = length

--Crie uma função que recebe uma lista de inteiros e retorne uma lista onde cada elemento seu é o dobro do respectivo elemento da lista de entrada.
listaDobro :: [Int] -> [Int]
listaDobro xs = [x * 2 | x <- xs]

--Crie uma função que receba uma lista de inteiros e retorne uma lista onde estejam somente os números pares da lista de entrada.
listaPares :: [Int] -> [Int]
listaPares xs = [x | x <- xs, even x]

--Faça uma função que receba uma lista e um inteiro n como entrada e retorne os n primeiros números da lista.
listaN :: [Int] -> Int -> [Int]
listaN xs n = if length xs <= n then xs else listaN (init xs) n

--Faça uma função que receba uma lista e um inteiro ncomo entrada e retorne os n primeiros números da lista. (Guardas)
listaNG :: [Int] -> Int -> [Int]
listaNG xs n
    |length xs <= n = xs
    |length xs > n = listaNG (init xs) n
    