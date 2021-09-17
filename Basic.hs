module Basic where

soma :: Int -> Int -> Int
soma x y = x + y

dobro :: Int -> Int
dobro x = x * 2

triplo :: Int -> Int
triplo x = x * 3

quadruplo :: Int -> Int
quadruplo x = x * 4

quadrado :: Int -> Int
quadrado x = x * x

quadobro :: Int -> Int
quadobro x = quadrado (dobro x)