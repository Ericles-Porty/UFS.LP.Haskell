module Quarta where

--4)Uma forma de criptografia antiga, chamada de trilho de ferrovia, funciona da seguinte
--forma: nela as letras da frase de entrada são separadas alternadamente em duas strings de
--forma que uma fica com as letras das posições pares e a outra com as letras das posições
--ímpares. Após esta etapa a segunda string é concatenada ao final da primeira. Ex: Considere a
--palavra de entrada: “abcdefghij” é separada em “acegi” e “bdfhj” sendo então a segunda lista
--concatenada ao final da primeira o que resulta em: “acegibdfhj”. Faça uma função que leia
--uma string e exiba-a criptografada com o método citado acima. (Valor 2,0)

listaUm :: String -> String
listaUm [] = []
listaUm [x] = [x]
listaUm (x : y : xs) = x : listaUm xs

listaDois :: String -> String
listaDois [] = []
listaDois [x] = []
listaDois (x : y : xs) = y : listaDois xs

cripto :: String -> String
cripto [] = []
cripto xs = listaUm xs ++ listaDois xs

main :: String -> String 
main = cripto
