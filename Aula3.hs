module Aula3 where

teste :: String -> String
teste (x:xs) = xs ++ [x]

testeNulo (x:y:[]) = [x]++[y] -- indica duas letras

teste3 (x:y:z:xs) = [x]++[y]++[z] --indica 3 letras

--Exercicio: Crie uma funcao que troque a segunda pela quarta letra e a primeira pela terceira.
troque :: String -> String
troque (a:b:c:d:xs) = [c]++[d]++[a]++[b]++xs
--Os value constroctors sao os valores dos data constructors. os value constroctors podem carregar campos. 
--record syntax
data Pessoa = Fisica {pessoaNome::String,
                        pessoaIdade::Int} | 
                Juridica {pessoaNome::String} deriving Show
--record syntax
--Um value constructor com o mesmo nome do data constructor
data Sozinho = Sozinho {a::String,
                        b::Char}
                        
--Fisica é um pattern matching que encaixa no tipo declarado. A variavel nome eh uma String e idade eh um Int
aniversario :: Pessoa -> Pessoa
aniversario (Fisica nome idade) = Fisica nome (idade+1)
aniversario (Juridica nome) = Juridica nome

--Exercicio: Faca a funcao fusao que concatene os campos
--nome e some os campos idade. A funcao deve funcionar para os dois values constructors.

fusao :: (Pessoa,Pessoa) -> Pessoa
fusao (Fisica nome idade, Fisica nom ida) = Fisica (nome++nom) (idade+ida)
fusao (Juridica nome, Juridica nom) = Juridica (nome++nom)

somarIdades::Pessoa -> Pessoa -> Int
somarIdades(Fisica _ i) (Fisica _ i1) = i+i1
somarIdades _ _ = 0

somarIdades' ::Pessoa -> Pessoa -> Int
somarIdades' x y = pessoaIdade x + pessoaIdade y

{-
Exercicio: Faca o tipo ponto com dois campos Double representando as posicoes x e y na tela.
Faca as funcoes:
moverX: Move um ponto dx unidades na direcao x.
A variavel dx eh um parametro.
moverY: move um ponto dy unidades na direcao y.
Avariavel dy eh um parametro.
mag: Extrai a distancia da origem de um ponto.
-}
data Ponto = Posicoes {cordenadaX::Double,
                        cordenadaY::Double} deriving Show
                
moverX:: Ponto -> Double -> Ponto
moverX x z = Posicoes (cordenadaX x+z) (cordenadaY x)

moverY :: Ponto -> Double -> Ponto
moverY x z = Posicoes (cordenadaX x) (cordenadaY x+z)

mag::Ponto -> Double
mag x = sqrt(cordenadaX x^2 + cordenadaY x^2)

