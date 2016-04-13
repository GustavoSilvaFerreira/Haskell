module Monitoria where

{-
1) Faca um tipo de estrutura que guarde dados de um aluno (curso e nome)
-}
data Aluno = Aluno {alunoCurso::String,
                    alunoNome::String} deriving Show
                    
guardaDados:: String -> String -> Aluno
guardaDados x y= Aluno (x) (y)

{-
2) Usando o fx anterior registre um aluno de (SI/ADS) através da funcao A x = ( Tipo "SI" x)
-}

regSiAluno:: String-> Aluno
regSiAluno x = Aluno "SI" x

regAdsAluno:: String-> Aluno
regAdsAluno x = Aluno "ADS" x