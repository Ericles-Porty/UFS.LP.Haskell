module Guardas where

import Think
  ( formaTriangulo,
    formaTrianguloEquilatero,
    formaTrianguloEscaleno,
    formaTrianguloIsoceles,
  )

--Escreva uma função que receba um número inteiro e imprima PAR ou ÍMPAR.
parOuImpar :: Int -> String
parOuImpar num
  | even num = "Par"
  | odd num = "Impar"

{-parOuImpar :: Int -> String
parOuImpar num
    |num `mod` 2 == 0 = "Par"
    |num `mod` 2 == 1 = "Impar"-}

--Escreva uma função que retorne uma mensagem dizendo se um número é múltiplo de algum dos números 2, 3, 5 ou de nenhum deles.
multiplos :: Int -> IO ()
multiplos x
  | mod x 2 == 0 = putStrLn "Multiplo de 2"
  | mod x 3 == 0 = putStrLn "Multiplo de 3"
  | mod x 5 == 0 = putStrLn "Multiplo de 5"
  | otherwise = putStrLn "Nao e multiplo de 2, 3 ou 5"

--Escreva uma função que receba 3 números inteiros e exiba o maior deles.
maiorDeTres :: Int -> Int -> Int -> Int
maiorDeTres a b c
  | a >= b && a >= c = a
  | b >= a && b >= c = b
  | c >= a && c >= b = c

--Usando as funções criadas nos exercícios anteriores crie uma função que receba três números e retorne um string informando o tipo de triângulo formado (obs: o triângulo formado pode ser equilátero, isósceles ou escaleno).

qualTriangulo :: Int -> Int -> Int -> IO ()
qualTriangulo a b c
  | formaTriangulo a b c && formaTrianguloEscaleno a b c = putStrLn "Escaleno!"
  | formaTriangulo a b c && formaTrianguloEquilatero a b c = putStrLn "Equilatero!"
  | formaTriangulo a b c && formaTrianguloIsoceles a b c = putStrLn "Isoceles!"
  | otherwise = putStrLn "Nao forma triangulo!"