module Funcoes where

--Uma função que receba um inteiro e calcule o cubo deste número.
cubo :: Int -> Int
cubo x = x ^ 3

--Crie uma função que leia o raio e exiba a área de uma circunferência.
areaCirculo :: Float -> Float
areaCirculo r = pi * (r ** 2)

--Crie uma função que receba comprimento e altura e exiba a área de um retângulo.
areaRetangulo :: Int -> Int -> Int
areaRetangulo b a = b * a