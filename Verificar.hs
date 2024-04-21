{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Verificar where
import Data.List (transpose)
import Data.Char (isDigit)

-- Função para verificar se existe uma linha de 4 caracteres iguais e não-zero na horizontal
verificaHorizontal :: [[Char]] -> Bool
verificaHorizontal matriz =  -- Recebe uma matriz de caracteres
    any (\linha ->  -- Checa se alguma linha na matriz...
        any (\sublista ->  -- ...contém uma sublista de 4 caracteres iguais a "yyyy" ou "rrrr"
            sublista == "yyyy" || sublista == "rrrr"
        ) (separarEmSublistas linha)  -- Divide a linha em sublistas de tamanho 4
    ) matriz  -- A matriz a ser verificada

-- Função para verificar se existe uma linha de 4 caracteres diferentes de '0' na vertical
-- Função para verificar se existe uma linha de 4 caracteres iguais na vertical
verificaVertical :: [[Char]] -> Bool
verificaVertical matriz =
    any (\coluna ->  -- Checa se alguma coluna, após transpor a matriz...
        any (\sublista ->  -- ...contém uma sublista de 4 caracteres iguais a "yyyy" ou "rrrr"
            sublista == "yyyy" || sublista == "rrrr"
        ) (separarEmSublistas coluna)  -- Divide a coluna em sublistas de tamanho 4
    ) (transpose matriz)  -- Transpõe a matriz para tratar as linhas como colunas

-- Função para obter todas as diagonais principais da matriz
diagonaisPrincipais :: [[Char]] -> [[Char]]
diagonaisPrincipais matriz = 
    [ [ matriz !! (i+k) !! (j+k)  -- Coleta as diagonais principais da matriz
        | k <- [0..n],  -- Itera pelos índices diagonais
          i+k < rows,  -- Garante que não ultrapasse as linhas da matriz
          j+k < cols  -- Garante que não ultrapasse as colunas da matriz
      ] 
    | i <- [0..rows-1],  -- Itera pelas linhas iniciais
      j <- [0..cols-1],  -- Itera pelas colunas iniciais
      i+j <= rows-4 || i+j <= cols-4  -- Apenas se a diagonal tiver pelo menos 4 elementos
    ]

  where
    rows = length matriz -- Número de linhas da matriz
    cols = length (head matriz) -- Número de colunas da matriz
    n = min (rows - 1) (cols - 1) -- Número de elementos diagonais

-- Função para obter todas as diagonais secundárias da matriz
diagonaisSecundarias :: [[Char]] -> [[Char]]
diagonaisSecundarias matriz =
    [ [ matriz !! (i+k) !! (j-k)  -- Coleta as diagonais secundárias da matriz
        | k <- [0..n],  -- Itera pelos índices diagonais
          i+k < rows,  -- Garante que não ultrapasse as linhas
          j-k >= 0  -- Garante que não ultrapasse as colunas
      ] 
    | i <- [0..rows-1],  -- Itera pelas linhas iniciais
      j <- [cols-1, cols-2..0],  -- Itera pelas colunas finais
      i <= rows-4 || j >= 3  -- Apenas se a diagonal tiver pelo menos 4 elementos
    ]
  where
    rows = length matriz  -- Número de linhas da matriz
    cols = length (head matriz)  -- Número de colunas da matriz
    n = min (rows - 1) (cols - 1)  -- Comprimento máximo para iterar nas diagonais

-- Função para verificar se existe uma linha de 4 caracteres iguais na diagonal
verificaDiagonal :: [[Char]] -> Bool
verificaDiagonal matriz = 
    any (\diagonal ->  -- Checa se alguma diagonal...
        any (\sublista ->  -- ...contém uma sublista de 4 caracteres iguais a "yyyy" ou "rrrr"
            sublista == "yyyy" || sublista == "rrrr"
        ) (separarEmSublistas diagonal)  -- Divide a diagonal em sublistas de tamanho 4
    ) diagonais  -- Todas as diagonais principais e secundárias
  where
    diagonais = diagonaisPrincipais matriz ++ diagonaisSecundarias matriz

-- Função para separar uma lista em sublistas de tamanho 4
separarEmSublistas :: [Char] -> [[Char]]
separarEmSublistas [] = []
separarEmSublistas lista
    | length lista >= 4 = take 4 lista : separarEmSublistas (drop 1 lista)
    | otherwise = []

-- Função para obter todas as diagonais principais e secundárias que tenham pelo menos 4 elementos
obterDiagonais :: [[Char]] -> [[Char]]
obterDiagonais matriz = 
    -- Concatena todas as diagonais principais e secundárias da matriz
    let todasAsDiagonais = diagonaisPrincipais matriz ++ diagonaisSecundarias matriz
    in
    -- Filtra as diagonais para retornar apenas aquelas com pelo menos 4 elementos
    filter ((>= 4) . length) todasAsDiagonais

-- Função para verificar se existe uma linha de 4 caracteres diferentes de '0' em qualquer direção
verificaLinhas :: [[Char]] -> Bool
verificaLinhas matriz = any verificaVitoria (matriz ++ transpose matriz ++ obterDiagonais matriz)

-- Função para verificar se existe uma sequência de 4 caracteres iguais e não-zero
verificaVitoria :: [Char] -> Bool
verificaVitoria lista = any (== "yyyy") (separarEmSublistas lista) || any (== "rrrr") (separarEmSublistas lista)

-- Função para verificar se todos os elementos de uma lista são zeros
todosZeros :: [Int] -> Bool
todosZeros [] = True -- Lista vazia, todos os elementos são zero
todosZeros (x:xs)
    | x == 0 = todosZeros xs -- Verifica o próximo elemento
    | otherwise = False -- Se encontrar um elemento diferente de zero, retorna False

-- Função para verificar se a string é um número
isNumber :: String -> Bool
isNumber str = all isDigit str
