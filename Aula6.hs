module Aula6 where

import Data.Monoid

data Bolsa a = Bolso a | Necessaire a a
-- a eh chamado de Type Parameter

data Item = Batom | Espelho | Celular | Chave deriving (Eq, Show)
-- Eq serve para tornar comparavel ex. Celular == Celular
-- :kind Item

--Eq eh um Type class, logo para cada tipo do Haskell, deve criar uma instancia para definir o comportamento desejado.
-- (Eq a) => restringe aos tipos a que sejam comparaveis
instance (Eq a) => Eq (Bolsa a) where
    (Bolso x) == (Bolso y) = x == y
    (Necessaire a b) == (Necessaire c d) = ((a==c)&&(b==d)||(a==d)&&(b==c))
    _==_ = False

instance (Show a) => Show (Bolsa a) where
    show (Bolso x) = "Uma linda bolsa: " ++ show x
    show (Necessaire a b) = "Uma linda bolsa: " ++ show a ++ " - " ++ show b
    
--Type class
class (Numero a) where
    numero:: a -> Int
    
instance Numero Item where
    numero Batom = 1
    numero Espelho = 132
    numero Celular = 123

{-
Faca uma funcao trocar, que troca as posicoes (contendo item) de dentro da necessaire para o bolso nao acontece nada.
-}

trocar:: Bolsa Item-> Bolsa Item
trocar (Necessaire x y)= Necessaire y x
trocar (Bolso x)= Bolso x
-- como Bolsa é kind(2) * -> *
-- deve especificar o tipo de dentro
-- nem que seja a, que representa qq tipo.

-- MONOIDE (data.Monoid) pegar com a Dinora

data Bolsa1 a = Vazio | Bolso1 a | Necessaire1 a a deriving Show

instance (Monoid a) => Monoid (Bolsa1 a) where
    mempty = Vazio
    mappend x Vazio = x
    mappend Vazio x = x
    mappend (Bolso1 a) (Bolso1 b) = Bolso1 (a <> b)
    mappend (Bolso1 a) (Necessaire1 b c) = Bolso1 (a <> b <> c)
    mappend (Necessaire1 b c) (Bolso1 a) = Bolso1 (a <> b <> c)
    mappend (Necessaire1 a b) (Necessaire1 c d) = Necessaire1 (a <> c) (b <> d)
    
    -- Teste 
    -- Bolso1 "Can " <> Bolso1 "Papel"
    
    -- mconcat v - Concatena todos os itens do vetor
    -- Ex. let v = [Bolso1 "Lapis ", Bolso1 "Caneta ",Bolso1 "Borracha "]
    -- mconcat v