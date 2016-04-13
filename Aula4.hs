module Aula4 where
--CURRYNG , LAMBDA , HIGH-ORDER FUNCTION
-- () -> Unidade (unity)
teste:: () -> Int
teste x = 0

teste2:: Int -> ()
teste2 x = ()

data Sozinho
-- no haskell todo tipo tem seu undefined
teste3:: Sozinho -> Int
teste3 x = 0

foo :: () -> Sozinho -> Int
foo x y =0
-- testar: foo () undefined

foo2 :: [Sozinho] -> Int
foo2 x = 0
-- testar: foo2 [undefined]

--CURRYING -> tecnica que consiste em transformar uma chamada de funcao em uma sequencia de funcoes com menos parametros que a original.
somar::Int -> Int -> Int
somar x y = x+y

volume:: Double -> Double -> Double -> Double
volume x y z = x*y*z

-- LAMBDA: Funcao anonima (sem corpo)
-- \p1 p2 p3 ... pn -> EXPR
-- Exemplo: (\x -> 2*x)
-- Lambda com currying
-- let somaa = (\x y -> x+y) 5

--HIGH-ORDER FUNCTION: (funcao de alta ordem) eh uma funcao que recebe via parametro(s) funcoes e/ou retorna funcoes.
aplicar::(Int->Int) -> Int
aplicar f = 1 + f 6

-- aplicar ((\x y -> x+y) 10)
-- = aplicar (\y -> 10+y)
-- 1 + (\y -> 10+y) 6
-- 1 + 16
-- 17

mult :: Int -> (Int -> Int)
mult x = (\y -> x*y)

-- map (\x -> x*2) [1,2,3,4,5]
-- map (2*) [1..5]

-- filter (\x -> length x == 5) ["ABCDE","FATEC","SANTOS","ABC"]
-- filter odd [1..15] = odd -> filtra os impares
-- filter even [1..15] = even filtra os pares
-- filter (\x -> elem x "aeiou") "fatec" = deixa so as vogais
-- filter (\x -> notElem x "aeiou") "fatec" = deixa as consoantes

data Pessoa = Pessoa String Int deriving Show

maior:: Pessoa -> Bool
maior (Pessoa _ idade) = idade >= 18

filtrarMaiores :: [Pessoa] -> [Pessoa]
filtrarMaiores xs = filter maior xs
-- let ps = [Pessoa "Gustavo" 28,Pessoa "Guil" 23,Pessoa "Fe" 17]
-- filtrarMaiores ps
-- filter (\(Pessoa _ idade) -> idade >= 18) [Pessoa "Gustavo" 28,Pessoa "Guil" 23,Pessoa "Fe" 17]

{-
a) Faca o tipo metragem com os value constructors Metro e Kilometro.
b) Faca o tipo medida que possua os campos Double e Metragem.
c) Faca a funcao converter que converte metros para Kilometros e vice-versa.
d) Faca a funcao converterTodos que converte todos os elementos de uma lista de medida.
e) Faca a funcao maior5 que filtra qualquer medida maior que 5 sem importar a metragem.
-}
data Metragem = Metro | Kilometro deriving Show
data Medida = Medida Double Metragem deriving Show

converter:: Medida -> Medida
converter (Medida x Metro) = Medida (x/1000) Kilometro
converter (Medida x Kilometro) = Medida (x*1000) Metro

converterTodos:: [Medida] -> [Medida]
converterTodos xs = map converter xs

maior5:: [Medida] -> [Medida]
maior5 xs = filter (\(Medida x _) -> x > 5 ) xs
--[Medida 20 Metro,Medida 10 Metro,Medida 5 Metro,Medida 4 Metro,Medida 30 Metro]