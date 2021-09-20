module Think where

--Uma função que receba um número inteiro e calcule o fatorial deste número.
fatorial :: Int -> Int
fatorial 1 = 1
fatorial n = n * fatorial (n - 1)

--Uma função que receba um número inteiro n e retorne o n-ésimo termo da sequência de Fibonacci.
fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib x = fib (x - 1) + fib (x - 2)

--Crie uma função que retorne o somatório dos números de 1 a n (considerando que n seja inteiro e positivo).
somatorio :: Int -> Int
somatorio n = sum [1 .. n]

somatorioRec :: Int -> Int
somatorioRec 1 = 1
somatorioRec n = n + somatorioRec (n-1)

--Uma função que receba 3 números inteiros e informe se eles são iguais.
comparaTres :: Int -> Int -> Int -> IO ()
comparaTres a b c = if a == b && b == c then putStrLn "Iguais" else putStrLn "Diferentes"

formaTriangulo :: Int -> Int -> Int -> Bool
formaTriangulo a b c = c < a + b && b < a + c && a < b + c

--Crie uma função que receba 3 números e informe se eles formam ou não triângulo.
formaTrianguloString :: Int -> Int -> Int -> String
formaTrianguloString a b c = if formaTriangulo a b c then "Forma Triangulo" else "Nao Forma Triangulo"

--Usando a função acima crie uma função que dados 3 números resulte em true se formarem triângulo e este triângulo for equilátero.
formaTrianguloEquilatero :: Int -> Int -> Int -> Bool
formaTrianguloEquilatero a b c = formaTriangulo a b c && a == b && b == c

--Usando uma ou mais das funções acima crie uma função que dados 3 números resulte em true se formarem triângulo se este triângulo for isósceles.
formaTrianguloIsoceles :: Int -> Int -> Int -> Bool
formaTrianguloIsoceles a b c = formaTriangulo a b c && (a == b) && a /= c || (a == c) && a /= b || (b == c) && b /= a

--Usando uma ou mais das funções acima crie uma função que dados 3 números resulte em true se formarem triângulo e este triângulo for escaleno.
formaTrianguloEscaleno :: Int -> Int -> Int -> Bool
formaTrianguloEscaleno a b c = formaTriangulo a b c && a /= b && b /= c

--Defina uma função que receba como argumento 3 números inteiros e retorne True apenas se os 3 forem diferentes.
diferentesTres :: Int -> Int -> Int -> Bool
diferentesTres a b c = a /= b && b /= c && a /= c

--Defina outra função que receba 4 números inteiros como parâmetro e retorne True somente se os 4 forem diferentes. Use a função anterior na criação desta.
diferentesQuatro :: Int -> Int -> Int -> Int -> Bool
diferentesQuatro a b c d = diferentesTres a b c && diferentesTres b c d && diferentesTres a b d && diferentesTres a c d