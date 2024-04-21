
module Verificar where
import Data.List (transpose)
import Data.Char (isDigit)

-- Função para verificar se existe uma linha de 4 caracteres diferentes de '0' na horizontal
verificaHorizontal :: [[Char]] -> Bool
verificaHorizontal matriz =
    any (\linha -> any (\sublista -> sublista == "yyyy" || sublista == "rrrr") (separarEmSublistas linha)) matriz

-- Função para verificar se existe uma linha de 4 caracteres diferentes de '0' na vertical
verificaVertical :: [[Char]] -> Bool
verificaVertical matriz =
    any (\coluna -> any (\sublista -> sublista == "yyyy" || sublista == "rrrr") (separarEmSublistas coluna)) (transpose matriz)

-- Função para obter todas as diagonais principais da matriz
diagonaisPrincipais :: [[Char]] -> [[Char]]
diagonaisPrincipais matriz = [ [ matriz !! (i+k) !! (j+k) | k <- [0..n], i+k < rows, j+k < cols ]
                              | i <- [0..rows-1], j <- [0..cols-1], i+j <= rows-4 || i+j <= cols-4]
  where
    rows = length matriz
    cols = length (head matriz)
    n = min (rows - 1) (cols - 1)

-- Função para obter todas as diagonais secundárias da matriz
diagonaisSecundarias :: [[Char]] -> [[Char]]
diagonaisSecundarias matriz = [ [ matriz !! (i+k) !! (j-k) | k <- [0..n], i+k < rows, j-k >= 0 ]
                                | i <- [0..rows-1], j <- [cols-1, cols-2..0], i <= rows-4 || j >= 3]
  where
    rows = length matriz
    cols = length (head matriz)
    n = min (rows - 1) (cols - 1)


-- Função para verificar se existe uma linha de 4 caracteres diferentes de '0' na diagonal
verificaDiagonal :: [[Char]] -> Bool
verificaDiagonal matriz =
    any (\diagonal -> any (\sublista -> sublista == "yyyy" || sublista == "rrrr") (separarEmSublistas diagonal)) diagonais
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
obterDiagonais matriz = filter ((>= 4) . length) $ diagonaisPrincipais matriz ++ diagonaisSecundarias matriz

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
