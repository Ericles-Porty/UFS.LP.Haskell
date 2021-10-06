module AltaOrdem where

mapear :: (a -> a) -> [a] -> [a]
mapear f [] = []
mapear f (a : b) = (f a) : (mapear f b)


reduzir :: [Int] -> Int
reduzir [] = 0
reduzir (x:xs) = x + reduzir xs

--Usando a função mapear e reduzir para uma lista de inteiros calcule o somatório do dobro dos elementos de uma lista de inteiros.
somaDoDobro :: (Num a) => [a] -> a
somaDoDobro [] = 0
somaDoDobro (x:xs) = x*2 + somaDoDobro xs

somaDoDobroProf :: [Int] -> Int
somaDoDobroProf xs = reduzir (mapear (*2) xs)
