module Tuplas where

--Retorna os n primeiros numeros de uma lista de produtos seguido de valor
listaN :: [(String,Float)] -> Int -> [(String,Float)]
listaN (x:xs) n = if length xs <= n then xs else x:listaN xs (n-1)