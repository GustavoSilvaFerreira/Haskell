module Tp where
import Control.Applicative
{-
Ruanyto - Gustavo 16/5 - 6.4 e 6.10  

Exercício 6.1
Faça um tipo Coisa com um type parameter a e três value constructors
chamados UmaCoisa, DuasCoisas e TresCoisas possuindo um, dois e três
campos de tipo a respectivamente.
-}
data Coisa a = Nada | UmaCoisa a | DuasCoisas a a | TresCoisas a a a deriving Show
{-
Exercício 6.3 Faça uma instância de Functor para o tipo Coisa definido no exercício 6.1.
A função deve ser aplicada em todas as coordenadas de Coisa.
-}
instance Functor Coisa where
    fmap f (UmaCoisa x) = UmaCoisa (f x)
    fmap f (DuasCoisas x y) = DuasCoisas (f x) (f y)
    fmap f (TresCoisas x y z) = TresCoisas (f x) (f y) (f z)
{-
Exercício 6.4
Aproveitando o exercício anterior, faça uma instância de Applicative
Functor para o tipo Coisa definido no exercício 6.1. Para definir um Applicative
Functor é necessário definir seu elemento neutro pure(TresCoisas) e seu
operador <*>. A regra para <*> deve ser a distribuição das funções dos campos
para o campo do argumento de forma ordenada. Por exemplo,

DuasCoisas (+4) (+5) <*> DuasCoisas 2 1 =
ghci> DuasCoisas 6 6                              (6.1)

Cada value constructor deve se combinar com o mesmo, 
caso contrário o valor retornado é sempre Nada.

Referência: http://learnyouahaskell.com/functors-applicative-functors-and-monoids
-}
instance Applicative Coisa where
    pure a = TresCoisas a a a
    (UmaCoisa a) <*> (UmaCoisa b) = UmaCoisa (a b)
    (DuasCoisas a b) <*> (DuasCoisas c d) = DuasCoisas (a c) (b d)
    (TresCoisas a b c) <*> (TresCoisas d e f) = TresCoisas (a d) (b e) (c f)
    _ <*> _ = Nada

data Produtos = Produtos {produtosNome :: String,
                            produtosValor :: Double} deriving Show
                            
{-                           
Produtos <$> (UmaCoisa "Algo") <*> (UmaCoisa 10)
Produtos <$> (DuasCoisas "Algo" "Algo2") <*> (DuasCoisas 10 20)
Produtos <$> (TresCoisas "Algo" "Algo2" "Algo3") <*> (TresCoisas 10 20 30)
Produtos <$> (UmaCoisa "Algo") <*> (DuasCoisas 10 20)
fmap Produtos (UmaCoisa "Algo") <*> (UmaCoisa 10)
TresCoisas (*5) (*5) (*5) <*> TresCoisas 5 5 6
TresCoisas (*5) (*5) (*5) <*> UmaCoisa 5
DuasCoisas (*5) (*5) <*> UmaCoisa 5
UmaCoisa (*5) <*> TresCoisas 5 5 6
-}

--instance Applicative Maybe where  
--    pure = Just  
--    Nothing <*> _ = Nothing  
--    (Just f) <*> something = fmap f something

-- QUADRO 4.2
data Arvore a = Nulo | Leaf a | Branch a (Arvore a) (Arvore a) deriving Show
{-
Exercício 6.10
Escreva uma instância para Functor e Applicative Functor para
o tipo (Arvore a) visto na Aula 4, quadro 4.2. A regra para estas instâncias são
análogas (a menos de recursão).
-}
instance Functor Arvore where
    fmap f Nulo = Nulo
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch x (Leaf y) (Leaf z)) = Branch (f x) (Leaf (f y)) (Leaf (f z))
    fmap f (Branch x Nulo (Leaf z)) = Branch (f x) Nulo (Leaf (f z))
    fmap f (Branch x (Leaf y) Nulo) = Branch (f x) (Leaf (f y)) Nulo
    fmap f (Branch x Nulo Nulo) = Branch (f x) Nulo Nulo
{-
fmap (+5) (Branch 10 (Leaf 5) (Leaf 5))
fmap (+5) (Branch 10 Nulo (Leaf 5))
fmap (+5) (Branch 10 (Leaf 5) Nulo)
fmap (+5) (Branch 10 Nulo Nulo)
(+5) <$> (Branch 10 (Leaf 5) Nulo)
-}
instance Applicative Arvore where
    pure a = Leaf a
    (Leaf a) <*> (Leaf b) = Leaf (a b)
    (Branch a (Leaf b) (Leaf c)) <*> (Branch d (Leaf e) (Leaf f)) = (Branch (a d) (Leaf (b e)) (Leaf (c f)))
    (Branch a Nulo (Leaf c)) <*> (Branch d Nulo (Leaf f)) = (Branch (a d) Nulo (Leaf (c f)))
    (Branch a (Leaf b) Nulo) <*> (Branch d (Leaf e) Nulo) = (Branch (a d) (Leaf (b e)) Nulo)
    (Branch a Nulo Nulo) <*> (Branch d Nulo Nulo) = (Branch (a d) Nulo Nulo)
    _ <*> _ = Nulo
    
{-
(Branch (+10) (Leaf (+5)) (Leaf (+5))) <*> (Branch 10 (Leaf 5) (Leaf 5))
(Branch (+10) Nulo (Leaf (+5))) <*> (Branch 10 Nulo (Leaf 5))
(Branch (+10) (Leaf (+5)) Nulo) <*> (Branch 10 (Leaf 5) Nulo)
(Branch (+10) Nulo Nulo) <*> (Branch 10 Nulo Nulo)
(Branch (+10) (Leaf (+5)) (Leaf (+5))) <*> Nulo
Nulo <*> (Branch 10 (Leaf 5) (Leaf 5))
-}