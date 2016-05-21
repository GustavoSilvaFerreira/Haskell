module Aula8 where
import Control.Monad

-- instance Monad Maybe where
--  return = Just
--  (Just x) >>= f = f x
--  Nothing >>= f = Nothing

-- neste caso a transformacao natural
-- abaixo pega um int e coloca sob acao de um funtor

transFnat::Int -> Maybe Int 
transFnat x = Just x

f::Int->[Int]
f x = [x+1]

-- join (fmap (transfNat (transfNat(3)))) =
-- join (fmap (transFnat [4]) = join [transFnat 4] = join

-- (f.g) 3 = f(g(3))
-- (f <=< g) 3 = join(fmap tnf (tng(3)))

-- <=< : chama-se KLEISLI ARROW e serve para compor o funtor - precisa import Control.Monad
-- OBS a <=< eh associativa

-- MONAD - eh um monoid na categoria dos endofuntores (objetos: transf Naturais)
-- se uma Monad eh um monoide
-- mampty = return
-- mappend = <=<

-- KLEISLI serve para composição de transformações naturais e o bind (>>=) serve aplicação de transf naturais em um
-- valor monádico (m b) o (>>=) tem a mesma função do ($), porem o ($) pode ser omitido ja o bind não
-- >> : ignora o primeiro parametro

expr:: Maybe Int
expr = Just 5 >>= \x -> Just (x+1) >>= \y -> Just (1+x) >>= \z -> Just (x+y+z)

expr' :: Maybe Int
expr' = do
      x <- Just 5
      y <- Just (x+1)
      z <- Just (2+x)
      return (x+y+z)
      
-- data () = ()
-- As setas sempre vem depois de algo IO ()
main:: IO ()
main = putStrLn "Digite um nome: " >>
        getLine >>= \x -> putStrLn ("Olá " ++x)

-- Na notação "do" nao ha uso de operadores monadicos
-- >>, >>=, =<<, ...
main' :: IO()
main' = do
        putStrLn "Digite um nome: "
        x <- getLine
        putStrLn ("olá " ++x)
        
main2 :: IO ()
main2 = do
      putStrLn "Digite um numero: "
      x <- readLn
      putStrLn ("O numero eh: " ++ show(x+1))
      
main3 :: IO ()
main3 = putStrLn "Digite um numero: " >>
        readLn >>= \x -> putStrLn("O numero eh: " ++show(x+1))

{-
1) faca um programa que receba string do teclado e mostre-o em ordem reversa (imperativo)

2) faca um programa que multiplique dois numeros (funcional)
-}

progUm:: IO()
progUm = putStrLn "Digite um nome: " >>
        getLine >>= \x -> putStrLn("Nome reverso: " ++ reverse(x))

progDois:: IO()
progDois = putStrLn "Digite um numero: " >>
            readLn >>= \x -> putStrLn "Digite outro numero: " >>
            readLn >>= \y -> putStrLn("O numero eh: " ++show(x*y))
            


-- Site consultas Haskell
--https://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Monad-Instances.html
--http://haskell.tailorfontela.com.br/a-fistful-of-monads
--http://haskell.tailorfontela.com.br/functors-applicative-functors-and-monoids#applicative-functors