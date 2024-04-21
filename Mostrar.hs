module Mostrar where
    
-- Função para imprimir uma lista de caracteres com quebra de linha
printLista :: [Char] -> IO ()
printLista [] = putStrLn "" -- Quando a lista estiver vazia, imprime uma nova linha
printLista (x:xs) = do
    putStr (x : " ") -- Imprime o elemento atual
    printLista xs -- Chama recursivamente para o restante da lista

-- Função para imprimir a matriz
printMatriz :: [[Char]] -> IO ()
printMatriz [] = return () -- Se a matriz estiver vazia, termina
printMatriz (x:xs) = do
    printLista x -- Imprime a linha atual
    printMatriz xs -- Chama recursivamente para o restante da matriz
