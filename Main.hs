import Verificar
import Mostrar

-- Função para armazenar um caractere em uma matriz e atualizar uma lista
armazena :: [[Char]] -> [Int] -> Char -> Int -> ([[Char]], [Int])
armazena matriz lista l c = 
    let
        -- Calcula a linha de inserção baseada na altura atual da coluna
        linhaInsercao = lista !! c

        -- Atualiza a matriz com o novo valor na posição correta
        novaMatriz = take linhaInsercao matriz ++ 
                     [take c (matriz !! linhaInsercao) ++ [l] ++ drop (c + 1) (matriz !! linhaInsercao)] ++ 
                     drop (linhaInsercao + 1) matriz

        -- Decrementa a posição da linha para a próxima inserção nessa coluna
        novaLista = take c lista ++ [linhaInsercao - 1] ++ drop (c + 1) lista
    in (novaMatriz, novaLista)


-- Função para criar uma matriz 6x7 preenchida com caracteres '0'
matrizZeros :: [[Char]]
matrizZeros = replicate 6 (replicate 7 '0')


-- Loop principal do jogo
main :: IO ()
main = loop matrizZeros (replicate 7 5) 'y' 'r'

loop :: [[Char]] -> [Int] -> Char -> Char -> IO ()
loop matriz lista jogador1 jogador2 = do
    putStrLn $ "Vez do jogador " ++ [jogador1] ++ ":"
    printMatriz matriz
    putStrLn "Escolha uma coluna (0-6):"
    colunaStr <- getLine
    if isNumber colunaStr
        then do
            let coluna = read colunaStr :: Int
            if coluna >= 0 && coluna < 7
                then do
                    if lista !! coluna >= 0  -- Verifica se a coluna ainda tem espaço
                        then do
                            let (matrizAtualizada, listaAtualizada) = armazena matriz lista jogador1 coluna
                            if verificaLinhas matrizAtualizada
                                then do
                                    putStrLn $ "Jogador " ++ [jogador1] ++ " venceu!"
                                    printMatriz matrizAtualizada
                                else if todosZeros listaAtualizada
                                    then do
                                        putStrLn "Empate!"
                                        printMatriz matrizAtualizada
                                    else loop matrizAtualizada listaAtualizada jogador2 jogador1
                        else do
                            putStrLn "Coluna cheia! Por favor, escolha outra coluna:"
                            loop matriz lista jogador1 jogador2
                else do
                    putStrLn "Número inválido, escolha uma coluna entre 0 e 6:"
                    loop matriz lista jogador1 jogador2
        else do
            putStrLn "Entrada inválida, por favor insira um número:"
            loop matriz lista jogador1 jogador2