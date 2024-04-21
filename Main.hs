import Verificar
import Mostrar

-- Função para armazenar um caractere em uma matriz e atualizar uma lista
armazena :: [[Char]] -> [Int] -> Char -> Int -> ([[Char]], [Int])
armazena matriz lista l c = 
    let
        -- Calcula a linha de inserção baseada na altura atual da coluna
        linhaInsercao = lista !! c

        -- Atualiza a matriz com o novo valor na posição correta
        -- A função `take` pega uma parte da matriz até a linha de inserção
        -- A função `drop` descarta uma parte da matriz após a linha de inserção
        novaMatriz = take linhaInsercao matriz ++ 
                     [take c (matriz !! linhaInsercao) ++ [l] ++ drop (c + 1) (matriz !! linhaInsercao)] ++ 
                     drop (linhaInsercao + 1) matriz

        -- Decrementa a posição da linha para a próxima inserção nessa coluna
        novaLista = take c lista ++ [linhaInsercao - 1] ++ drop (c + 1) lista
    in (novaMatriz, novaLista)


-- Função para criar uma matriz 6x7 preenchida com caracteres '0'
-- Representa o tabuleiro vazio do jogo
matrizZeros :: [[Char]]
matrizZeros = replicate 6 (replicate 7 '0')  -- Cria uma matriz de 6 linhas e 7 colunas preenchida com '0'

-- Loop principal do jogo
main :: IO ()
main = loop matrizZeros (replicate 7 5) 'y' 'r'

loop :: [[Char]] -> [Int] -> Char -> Char -> IO ()
loop matriz lista jogador1 jogador2 = do
    -- Solicita a vez do jogador e exibe o tabuleiro
    putStrLn $ "Vez do jogador " ++ [jogador1] ++ ":"
    printMatriz matriz

    -- Solicita a coluna para inserir a peça
    putStrLn "Escolha uma coluna (0-6):"
    colunaStr <- getLine

    -- Verifica se a entrada é um número
    if isNumber colunaStr
        then do
            let coluna = read colunaStr :: Int -- Converte a entrada para inteiro
            if coluna >= 0 && coluna < 7 -- Verifica se a coluna está dentro do intervalo
                then do
                    if lista !! coluna >= 0  -- Verifica se a coluna ainda tem espaço
                        then do
                            -- Armazena a jogada na matriz e atualiza a lista
                            let (matrizAtualizada, listaAtualizada) = armazena matriz lista jogador1 coluna
                            -- Verifica se a jogada venceu o jogo
                            if verificaLinhas matrizAtualizada
                                then do
                                    putStrLn $ "Jogador " ++ [jogador1] ++ " venceu!"
                                    printMatriz matrizAtualizada

                                -- Verifica se todas as colunas estão cheias (empate)
                                else if todosZeros listaAtualizada
                                    then do
                                        putStrLn "Empate!"
                                        printMatriz matrizAtualizada
                                    else loop matrizAtualizada listaAtualizada jogador2 jogador1
                                         -- Se ninguém venceu, chama o loop novamente com a matriz e lista atualizadas
                        else do
                            putStrLn "Coluna cheia! Por favor, escolha outra coluna:"
                            loop matriz lista jogador1 jogador2
                else do
                    putStrLn "Número inválido, escolha uma coluna entre 0 e 6:"
                    loop matriz lista jogador1 jogador2
        else do
            putStrLn "Entrada inválida, por favor insira um número:"
            loop matriz lista jogador1 jogador2