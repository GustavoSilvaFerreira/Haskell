module Aula1 where
-- Para fazer uma função haskell
--nome x1 x2 x3 x4 ...xn = expressão

--dobro HAS TYPE Int no parametro e Int na saída
dobro :: Int -> Int
dobro x = 2*x
somaUm :: Int -> Int
somaUm x = x+1
-- O último tipo sempre é da saída

somar :: Int -> Int -> Int
somar x y = x+y
v :: [Int]
v = [2,3,4,5,6]

w :: [Char]
w = ['a'..'g']

s :: String
s = "Gustavo"

--List Compreenshions
--[EXPR | LISTA DE ITERAÇÃO, FILTRO, FILTRO,...]
todosPares :: Int -> [Int]
todosPares n = [x | x <- [0 .. n], mod x 2 == 0]
multCinco :: Int -> [Int]
multCinco n = [x | x <- [0 .. n], mod x 5 == 0]

--O dobro de todos os numeros de 0 a n maiores que 12 e menores que 142.
pares :: Int -> [Int]
pares n = [x*2 | x <-[0 .. n],x > 12, x < 142]

