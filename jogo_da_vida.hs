{- 
Tradução dos valores:
    0 -> célula morta
    1 -> célula viva
    2 -> célula zumbi
-}

printBoard :: [[Int]] -> IO ()
printBoard [] =
    putStrLn ""

printBoard (head : tail) = do
    putStrLn $ unwords $ map show head
    printBoard tail

-- Verifica se a posição i j pertence ao tabuleiro
isValidPosition :: Int -> Int -> Int -> Bool
isValidPosition i j n = 
    i >= 0 && i < n && j >= 0 && j < n

-- Recebe i j newValue board e retorna newBoard
-- Irá modificar o elemento na posição i j para newValue da matriz board e irá retornar newBoard
updateCell :: Int -> Int -> Int -> [[Int]] -> [[Int]]
updateCell i j newValue board = 
    take i board ++ [take j (board !! i) ++ [newValue] ++ drop (j + 1) (board !! i)] ++ drop (i + 1) board

-- Função que le a matriz de tamanho n x n recursivamente
-- Recebe o parametro "n" em que determina a quantidade de linhas restantes a serem inseridas, a cada iteração irá diminuir até zerar
readMatrix :: Int -> IO [[Int]]

-- Quando chegar a 0 linhas restantes irá parar.
readMatrix 0 = return []

readMatrix n = do
    head <- map read.words<$> getLine  -- Le a linha como um vetor de inteiros
    tail <- readMatrix (n-1) -- Continua a recursão
    return (head : tail) -- Concatena as matrizes, onde head é a linha atual e tail é as linhas restantes da matriz

-- Função que recebe uma posição [i][j], um tamanho n e um tabuleiro "board" e retorna o conteudo dessa célula
checkCell :: Int -> Int -> Int -> [[Int]] -> Int

checkCell i j n board =
    if isValidPosition i j n
        then board !! i !! j
        else -1

-- Recebe um vetor e retorna um vetor contando as repetições dos números 0, 1 e 2
countArray :: [Int] -> [Int]
countArray = foldr countElement [0, 0, 0]
  where
    countElement :: Int -> [Int] -> [Int]
    countElement 0 [c0, c1, c2] = [c0 + 1, c1, c2]
    countElement 1 [c0, c1, c2] = [c0, c1 + 1, c2]
    countElement 2 [c0, c1, c2] = [c0, c1, c2 + 1]
    countElement _ counts = counts
    
-- Função que recebe uma posição [i][j], um tamanho n e um tabuleiro "board" e irá retornar um vetor contendo a quantidade de células mortas, vivas e zumbis vizinhas no formato [# mortas, # vivas, # zumbis]
countNearbyCells :: Int -> Int -> Int -> [[Int]] -> [Int]

countNearbyCells i j n board = do
    let pos1 = checkCell (i-1) (j-1) n board
    let pos2 = checkCell (i-1) (j) n board
    let pos3 = checkCell (i-1) (j+1) n board
    let pos4 = checkCell (i) (j-1) n board
    let pos5 = checkCell (i) (j+1) n board
    let pos6 = checkCell (i+1) (j-1) n board
    let pos7 = checkCell (i+1) (j) n board
    let pos8 = checkCell (i+1) (j+1) n board
    let positions = [pos1, pos2, pos3, pos4, pos5, pos6, pos7, pos8]
    countArray positions

-- Atualiza o tabuleiro recursivamente, verificando posição por posição
updateBoard :: Int -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
updateBoard i j n board newBoard =
    if isValidPosition i j n
        then
            let neighbor = countNearbyCells i j n board
                currCell = board !! i !! j
                newCell = 
                    case (currCell, neighbor !! 1, neighbor !! 2, (neighbor !! 1 < 2 || neighbor !! 1 > 3), (neighbor !! 2 >= 1)) of
                        (0, 3, _, _, _) -> 1 -- Celula morta e possui 3 celulas vivas adjacentes (ressucita)
                        (1, _, _, _, True) -> 2 -- Celula viva e possui pelo menos 1 zumbi adjacente (zumbificação)
                        (1, _, 0, True, False) -> 0 -- Celula viva e não possui zumbi adjacente e possui menos que 2 ou mais que 3 vivas adjacentes (morre)
                        (2, 0, _, _, _) -> 0 -- Celula zumbi sem celula viva adjacente (morre)
                        (_, _, _, _, _) -> currCell -- Caso não se encaixe em nenhum caso, ela não mudará
            
            in updateBoard i (j+1) n board (updateCell i j newCell newBoard)

        else if isValidPosition (i+1) 0 n
            then updateBoard (i+1) 0 n board newBoard
            else newBoard
    
-- Função que irá rodar uma quantidade n de turnos (iterações)
-- Recebe n (numero de interações), size (tamanho do tabuleiro) e board (tabuleiro) como parametros e retorna o tabuleiro após as n interações
playTurns :: Int -> Int -> [[Int]] -> ([[Int]], Int)
playTurns 0 size board = (board, 0)

playTurns n size board =
    let newBoard = updateBoard 0 0 size board board
    in if board == newBoard
        then (board, n)
        else playTurns (n-1) size newBoard

main :: IO ()
main = do
    sizeInput <- getLine
    let size = read sizeInput :: Int
    board <- readMatrix size  -- Cria o tabuleiro na forma matricial
    turnsInput <- getLine
    let n = read turnsInput :: Int
    let result = playTurns n size board
    putStrLn "Tabuleiro resultante:"
    printBoard (fst result)
    if (snd result) > 0
        then
            putStrLn $ "Tabuleiro estabilizado após " ++ show (n - snd result) ++ " iterações"
        else putStrLn "Tabuleiro não estabilizou antes de finalizar os turnos"
    
    putStrLn "\nLegenda:"
    putStrLn "0 - Célula morta"
    putStrLn "1 - Célula viva"
    putStrLn "2 - Célula zumbi"
    