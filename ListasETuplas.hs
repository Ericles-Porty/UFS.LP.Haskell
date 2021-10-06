module ListasETuplas where

import Guardas (qualTriangulo)
import Think (formaTriangulo)

--Faça uma função que receba uma lista de inteiros e exiba a lista invertida. Ex: [1,2,3,4] sairá [4,3,2,1]
invertido :: [Int] -> [Int]
invertido [] = []
invertido (x : xs) = invertido xs ++ [x]

--Faça uma função que receba um elemento e uma lista e retorne verdadeiro caso o elemento pertença a lista.
pertence :: [Int] -> Int -> Bool
pertence [] _ = False
pertence (x : xs) n = (x == n) || pertence xs n

--Faça uma função que receba um inteiro n qualquer e uma lista de inteiros e retorne outra lista de inteiros onde cada número está multiplicado pelo inteiro passado.
multN :: [Int] -> Int -> [Int]
multN [] _ = []
multN xs n = [x * n | x <- xs]

--Faça uma função que leia uma lista de inteiros e retorne o maior elemento da lista.
maiorDaLista :: [Int] -> Int
maiorDaLista [] = 0
maiorDaLista (x : xs) = if x > maiorDaLista xs then x else maiorDaLista xs

--Crie uma função que receba uma lista com diversos produtos e um valor n e retorne os n primeiros produtos.
nProdutos :: [(String, Float)] -> Int -> [(String, Float)]
nProdutos xs n = if length xs > n then nProdutos (init xs) n else xs

--Construa uma função que dado três valores verifique se eles podem formar um triângulo, caso seja verdadeiro retorne uma tupla-2 com o tipo do triângulo formado e o perímetro do mesmo.
triPeri :: Int -> Int -> Int -> (String, Int)
triPeri a b c = if formaTriangulo a b c then (qualTriangulo a b c, a + b + c) else ("Nao forma Triangulo", 0)

--Crie uma função que leia uma lista contendo nomes de produtos e seus preços e exiba o valor total da compra.
valorDaCompra :: [(String, Float)] -> Float
valorDaCompra [] = 0
valorDaCompra ((a, b) : ab) = b + valorDaCompra ab

--Adapte a função anterior para ler também a quantidade de cada produto comprado e exibir o valor total da compra.
valorDasCompras :: [(String, Float, Float)] -> Float
valorDasCompras [] = 0
valorDasCompras ((a, b, c) : abc) = b * c + valorDasCompras abc

--Faça uma função que leia o nome do produto, valor unitário e quantidade comprada de uma lista de produtos e exiba:
--Uma lista com o nome do produto e o valor parcial comprado de cada produto.
carrinho :: [(String, Float, Float)] -> [(String, Float)]
carrinho [] = []
carrinho ((a, b, c) : abc) = (a, b * c) : carrinho abc

-- [("Pao",0.33,4)]

total :: [(String, Float, Float)] -> Float
total [] = 0
total ((a, b, c) : abc) = b * c + total abc

--Após a lista deve ser exibido o valor total da compra.
carrinhoTotal :: [(String, Float, Float)] -> ([(String, Float)], Float)
carrinhoTotal abc = (carrinho abc, total abc)

--Crie, por lint compregension, uma função que retorne o dobro dos múltiplos de 3 que estão entre 50 e 100.
lintCompregension :: [Int]
lintCompregension = [x | x <- [50 .. 100], mod x 3 == 0]

--Leia uma lista de inteiros e usando list comprehension exiba o triplo de cada elemento da lista.
listTriple :: [Int] -> [Int]
listTriple xs = [x * 3 | x <- xs]

--Exiba o quadrado dos múltiplos de 3 menores que 50.
listQuadrada :: [Int]
listQuadrada = [x ^ 2 | x <- [3 .. 50], mod x 3 == 0]

--Exiba tuplas com as possíveis combinações entre as letras de um string lido e os números de 1 a 5, como no exemplo a seguir:
parteLista :: Char -> [Int] -> [(Char, Int)]
parteLista _ [] = []
parteLista x (a:bs) = (x, a): parteLista x bs

combinaListas :: String -> [[(Char, Int)]]
combinaListas [] = []
combinaListas (x:xs) = parteLista x [1..5]: combinaListas xs

--Usando list comprehension crie uma função que exiba todos os múltiplos de 3 entre naturais menores que 100.
listTripleNatural :: [Int]
listTripleNatural = [x | x <- [0 .. 100], mod x 3 == 0]

--Usando list comprehension crie uma função que exiba todos os múltiplos de n que estejam presentes em uma lista de inteiros lida.
listMultipleN :: [Int] -> Int -> [Int]
listMultipleN xs n = [x | x <- xs, mod x n == 0]

--Usando list comprehension crie uma função que calcule a soma de 1² + 2² +...+50²
listSquareSum :: Int
listSquareSum = sum [x ^ 2 | x <- [1 .. 50]]