module Aula5 where

-- Recursivo
--Zero ( 0 ) eh um value constructor de Integer e nao condicao booleana
-- Integer da uma precisao maior
fat:: Integer -> Integer
fat 0 = 1
fat k = k* fat (k-1)


-- " | " se chama GUARDS: Serie de verificações booleanas executadas em ordem ate chegar em otherwise
-- pode ter varias condições
fat'::Integer -> Integer
fat' k
    | k <= 1 = 1
    | otherwise = k * fat' (k-1)

--fibonacci
fib::Integer -> Integer
fib n
    | n <= 2 = 1
    | otherwise = fib (n-1) + fib (n-2)
--teste: map fib [1..10]

eliminarVogais:: String -> String
eliminarVogais [] = []
eliminarVogais (x:xs)
                | elem x "AEIOUaeiou" = eliminarVogais xs
                | otherwise = x : eliminarVogais xs

{-
Exercicios
1) Faca recursivamente o que se pede:
    a) Elimine todas as palavras de 4 letras de uma lista de Strings e faca o teste de mesa.
    b) Elimine todos os pares 'AA' de uma String e faca o teste de mesa.
-}

recPalavra:: [String] -> [String]
recPalavra [] = []
recPalavra (x:xs)
            | length x == 4 = recPalavra xs
            | otherwise = x : recPalavra xs
            
elimParesAA:: String -> String
elimParesAA [] = []
elimParesAA (x:y:xs)
            | elem x "A" && elem y "A" = elimParesAA xs
            | otherwise = x:elimParesAA (y:xs)
elimParesAA (x:xs) = x:xs

foo:: String -> String
foo []=[]
foo (x:y:z:xs)
    | x == 'A' && y == 'B' && z == 'C' = foo xs
    | otherwise = x : foo (y:z:xs)
foo x = x
{-
teste mesa palavra: foo "DABCD"

foo "DABCD"
foo D A B CD =  D : foo A B CD
foo A B C D = foo D
foo D = D
foo DD
-}

-- reduce no haskell é FOLDL
-- foldl (+) 0 [1,2,3,4]
-- foldl (\x y -> x+ length y) 0 ["f","f","f"]
-- foldl (*) 1 [1,2,3,4]

{-foldl (\x y -> EXPR ENVOLVENDO x e/ou y) inicial lista
x representa a acumulacao do valor inicial com o resultado da expressao
y eh alguem da lista
(voce sempre pode ignorar, se necessario, x ou y do lambda)
(\ _ y -> EXPR)
(\ x _ -> EXPR)
O FOLDL sempre usa uma funcao de dois parametros
-}

{-
EXERCICIO:
a) faca um foldl para contar letras de uma palavra
b) faca um foldl para reverter uma string
-}
-- foldl (\x _ -> x+1) 0 "Fatec"
-- foldl (\x y -> [y]++x) "" "Fatec"