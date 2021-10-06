module Polimorfismo where

import Distribution.Simple.Utils (xargs)

--Faça uma função polimórfica que dadas duas listas de qualquer tipo dê como resultado uma lista de tuplas-2 onde o primeiro elemento vem da primeira lista e o segundo vem da segunda lista.
junta :: [a] -> [b] -> [(a, b)]
junta [] [] = []
junta [] _ = []
junta _ [] = []
junta (x : xs) (y : ys) = (x, y) : junta xs ys

--Faça uma função polimórfica que dada uma lista de qualquer tipo e um número inteiro n retorne uma lista com os n primeiros elementos da lista passada.
nPrimeirosLista :: [a] -> Int -> [a]
nPrimeirosLista [] _ = []
nPrimeirosLista _ 0 = []
nPrimeirosLista (x : xs) n = x : nPrimeirosLista xs (n - 1)

--Faça uma função polimórfica que dada uma lista qualquer, sem usar o last, retorne o último elemento da lista passada.
nUltimoLista :: [a] -> [a]
nUltimoLista [x] = [x]
nUltimoLista (x : xs) = nUltimoLista xs

--Crie uma função polimórfica que receba uma lista de um determinado tipo e retorne o menor valor da lista.
menorComClasse :: (Ord a) => [a] -> a
menorComClasse [x] = x
menorComClasse [] = error "Deu erro lista vazia"
menorComClasse (x : y : xs)
  | x < y = menorComClasse (x : xs)
  | otherwise = menorComClasse (y : xs)

--Crie uma função polimórfica que receba como parâmetro uma lista de números, sejam inteiros ou reais e retorne o somatório destes números.
somaListaClasse :: (Num a) => [a] -> a
somaListaClasse [] = 0
somaListaClasse [x] = x
somaListaClasse (x : xs) = x + somaListaClasse xs

--Crie uma função polimórfica que receba limite inicial e final de um intervalo contínuo de inteiros, reais ou char e retorne quantos elementos há neste intervalo, incluindo os limites. Obs: Os números reais são contados a partir do número inicial com incremento 1.
contarIntervaloClasse :: (Enum a) => a -> a -> Int
contarIntervaloClasse x y = length [x .. y]

