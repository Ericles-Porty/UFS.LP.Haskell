module Tres where

--Uma forma de representar polinômios de uma variável é usar listas de pares (coeficiente,
--expoente), assim um polinômio vai ser representado como uma lista de tuplas [(Float,Int)].
--Faça uma função que receba um polinômio não simplificado, entretanto com termos
--ordenados, como entrada e dê como saída esse polinômio simplificado. (Valor 2,5)
--Ex de entrada: [(3,1),(2,1),(1,2),(5,2),(1,2),(3,4)]
--Ex do que seria a saída: [(5,1),(7,2),(3,4)]

simplifica2 :: [(Float, Int)] -> [(Float, Int)]
simplifica2 [] = []
simplifica2 [x] = [x]
simplifica2 ((a, b) : (x, y) : xs)
  | b == y = (a + x, b) : simplifica2 xs
  | otherwise = (a, b): simplifica2 [(x, y)] ++ xs

simplifica3 :: [(Float, Int)] -> [(Float, Int)]
simplifica3 [] = []
simplifica3 [x] = [x]
simplifica3 [x,y] = [x,y]
simplifica3 ((a, b) : (x, y) : (c,z): xs)
  | y == z = (a,b): (x + c, z) : simplifica3 xs
  | otherwise = (a, b): simplifica3 [(x, y)]++[(x,z)] ++ xs

repete :: Int -> [(Float, Int)] -> [(Float, Int)]
repete i xs
  | i > 0 = repete (i - 1) (simplifica3(simplifica2 xs))
  | otherwise = xs

main :: [(Float, Int)] -> [(Float, Int)]
main xs = repete 10 xs
