module Aula2 where

-- "Tipo" em haskell a primeira letra sempre é maiúscula
-- Dia é chamado de data constructor
-- E o lado direito sao chamados de value constructors
data Dia= Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo deriving Show
-- :t retorna o tipo

{- 
Pattern Matching
Encontra um padrao específico na(s) entrada(s) da função. 
Age de acordo com os values constructors do tipo de entrada
-}
diaDeBalada :: Dia -> Bool
diaDeBalada Sabado = True
diaDeBalada Sexta = True
diaDeBalada _ = False

numDia :: Dia -> Int
numDia Domingo = 1
numDia Segunda = 2
numDia Terca = 3
numDia Quarta = 4
numDia Quinta = 5
numDia Sexta = 6
numDia Sabado = 7

{-
Faca uma função chamada salario que implemente a seguinte regra de negocio:
- todo trabalhador aos domingos ganha o dobro do salario
- Aos sabados 75% a mais
- Qualquer outro dia nao ha um acrescimo
A funcao deve receber um dia e o valor bruto do salario por hora a ser calculado
-}

salario:: Dia -> Double -> Double
salario Domingo x = x*2
salario Sabado x= x*1.75
salario _ x = x
{-
Faca o tipo Day que possua como value constructors os dias da semana em ingles
Faca as funcoes traduzirIp que traduz os dias em ingles para portugues e traduzirPi que traduz
do portugues para o ingles
-}
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving Show

traduzirIp:: Day -> Dia
traduzirIp Monday = Segunda
traduzirIp Tuesday = Terca
traduzirIp Wednesday = Quarta
traduzirIp Thursday = Quinta
traduzirIp Friday = Sexta
traduzirIp Saturday = Sabado
traduzirIp Sunday = Domingo

traduzirPi:: Dia -> Day
traduzirPi Segunda = Monday
traduzirPi Terca = Tuesday
traduzirPi Quarta = Wednesday
traduzirPi Quinta = Thursday
traduzirPi Sexta = Friday
traduzirPi Sabado = Saturday
traduzirPi Domingo = Sunday

--Tuplas Podem receber qualquer conteudo

mult::(Int,Int) -> Int
mult x = fst x * snd x

mult' :: (Int,Int) -> Int
mult' (x,y) = x*y

{-
1) Faca uma funcao que receba um inteiro e eh retornado o dobro deste inteiro- na primeira cordenada de uma tupla,
o triplo na segunda e o quadruplo na terceira.

2)Faca o tipo Pessoa que pode ser Fisica ou Juridica, e o Tipo Imposto que pode ser ISS, IRPF ou ICMS. Faca a funcao devePagar
que recebe uma tupla de Pessoa e Imposto. Essa funcao informa se esta Pessoa deve pagar ou nao este Imposto.
-}

recInt:: Int -> (Int,Int,Int)
recInt x = (x*2,x*3,x*4)

data Pessoa = Fisica | Juridica
data Imposto = ISS | IRPF | ICMS

devePagar::(Pessoa,Imposto) -> Bool
devePagar (Fisica,IRPF) = True
devePagar (Juridica,ISS) = True
devePagar (Juridica,ICMS) = True
devePagar _ = False