module ListComprehension where

adicionaUm :: [Int] -> [Int]
adicionaUm xs = [x + 1 | x <- xs]

adicionaUmNosImpares :: [Int] -> [Int]
adicionaUmNosImpares xs = [x + 1 | x <- xs, odd x]

adicionaUmNosPares :: [Int] -> [Int]
adicionaUmNosPares xs = [x + 1 | x <- xs, even x]

gerarListaDePares :: [Int]
gerarListaDePares = [x| x <- [0..20], even x]

gerarListaDeImpares :: [Int]
gerarListaDeImpares = [x| x <- [0..20], odd x]

gerarLista :: [Int]
gerarLista = [1 .. 10] ++ [x | x <- [10, 12 .. 20], even x]

gerarListaDecimal :: [Double]
gerarListaDecimal = [x * 0.1 + 1| x <- [1..7]]
