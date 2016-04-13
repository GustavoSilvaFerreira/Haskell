module Aula7 where
-- Categorias e Funtores
data Humano = Bia | Pedro deriving Show

data Vampiro a = Vampiro a deriving Show

instance Functor Vampiro where
    fmap f (Vampiro x) = Vampiro (f x)

amor:: Humano -> Humano
amor Bia = Pedro
amor Pedro = Bia

--data Valido a = Sim a | Nao deriving Show
--data Maybe a = Just a | Nothing

{-
instance (Show a) => Show (Valido a) where
    Show (Sim x) = x
    Show Nao = "Erro..."
-}
(/?) :: Double -> Double -> Maybe Double
(/?) x 0 = Nothing
(/?) x y = Just (x/y)

--Transformacoes naturais
toJust:: a -> Maybe a
toJust x = Just x

toList:: a -> [a]
toList x = [x]