import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
-- import Data.Text.Conversions as Text

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Char
import Data.List

import Text.Printf

load :: String -> IO ()
load name = do
    xs <- fmap Text.lines (Text.readFile name)
    putStrLn "Read map successfully!"
    putStrLn "Initial:"
    putBoard (map (Text.unpack) xs)
    putStr ">"
    option <- getLine
    if option == "play"
        then
            do
                moves <- getDirections [] 0
                play moves (map (Text.unpack) xs) 0
    else
        putStrLn "INVALID"

currBallPos :: [String]  -> (Int,Int)
currBallPos xs = if length positions == 1 then head positions else (-1,-1)
                    where positions = getPos xs '@'

play :: [String] -> [String] -> Int -> IO ()
play [] _ _ = return ()
play (move:moves) board bonus = do
    putBoard (fst newBoard)
    if (snd newBoard) > bonus
        then
            putStrLn "Got a bonus"
        else
            putStrLn "No bonus"
    putStrLn ""
    play moves (fst newBoard) (snd newBoard)
        where newBoard = makeMove board move bonus

getPos :: [String] -> Char -> [(Int, Int)]
getPos xs c = [(i,j) | (i,l) <- (zip [0..] xs), j <- elemIndices c l]

makeMove :: [String] -> String -> Int -> ([String], Int)
makeMove board move bonus
 | move == "Right"  = ((take row board ++ [fst(rightRow)] ++ drop (row+1) board), (snd rightRow))
 | move == "Left"   = ((take row board ++ [fst(leftRow)] ++ drop (row+1) board), (snd leftRow))
 | move == "Up" = moveUp board row idx '-' bonus
 | move == "Down" = moveDown board row idx '-' bonus
 | otherwise    = error "Move invalid"
    where   rightRow = moveRight (board !! row) idx '-' bonus
            leftRow = moveLeft (board !! row) idx '-' bonus
            row = (fst (currBallPos board))
            idx = (snd (currBallPos board))

moveRight :: String -> Int -> Char ->  Int -> (String, Int)
moveRight line position prev bonus
 | (position > ((length line) - 3)) = (line, bonus)
 | not ((line !! (position + 2)) `elem` ['-', 'b'])    = (line, bonus)
 | (line !! (position + 2)) == 'b'  = moveRight (moveOneRight line position prev '@') (position + 2) (line !! (position +2)) (bonus+1)
 | position < (length line)   = moveRight (moveOneRight line position prev '@') (position + 2) (line !! (position +2)) bonus
 | otherwise    = (line, bonus)

moveLeft :: String -> Int -> Char -> Int -> (String, Int)
moveLeft line position prev bonus
 | (position < 2)   = (line,bonus)
 | not ((line !! (position - 2)) `elem` ['-', 'b'])    = (line, bonus)
 | (line !! (position - 2)) == 'b'   = moveLeft (moveOneLeft line position prev '@') (position - 2) (line !! (position - 2)) (bonus+1)
 | position < (length line)   = moveLeft (moveOneLeft line position prev '@') (position - 2) (line !! (position - 2)) bonus
 | otherwise    = (line,bonus)  

moveUp :: [String] -> Int -> Int -> Char -> Int -> ([String], Int)
moveUp board row position prev bonus
 | (row < 1)    = (board, bonus)
 | not (((board !! (row-1)) !! position) `elem` ['-', 'b'])    = (board, bonus)
 | ((board !! (row-1)) !! position) == 'b'   = moveUp (moveOneUp board row position '@' prev) (row-1) position ((board !! (row-1)) !! position) (bonus +1)
 | otherwise   = moveUp (moveOneUp board row position '@' prev) (row-1) position ((board !! (row-1)) !! position) bonus

moveDown :: [String] -> Int -> Int -> Char -> Int -> ([String], Int)
moveDown board row position prev bonus
 | (row >= (length board)-1)    = (board, bonus)
 | not (((board !! (row+1)) !! position) `elem` ['-', 'b'])    = (board, bonus)
 | ((board !! (row+1)) !! position) == 'b'  = moveDown (moveOneDown board row position '@' prev) (row+1) position ((board !! (row+1)) !! position) (bonus+1)
 | otherwise   = moveDown (moveOneDown board row position '@' prev) (row+1) position ((board !! (row+1)) !! position) bonus

moveOneRight :: String -> Int -> Char -> Char -> String
moveOneRight l pos x y 
 | x /= 'b' = take pos l ++ [x] ++ " " ++ [y] ++ drop (pos+3) l
 | otherwise    = take pos l ++ ['-'] ++ " " ++ [y] ++ drop (pos+3) l

moveOneLeft :: String -> Int -> Char -> Char -> String
moveOneLeft l pos x y
 | x /= 'b' = take (pos-2) l ++ [y] ++ " " ++ [x] ++ drop (pos+1) l
 | otherwise = take (pos-2) l ++ [y] ++ " " ++ ['-'] ++ drop (pos+1) l

moveOneUp :: [String] -> Int -> Int -> Char -> Char -> [String]
moveOneUp board row idx new prev
 | prev /= 'b' = (take (row-1) board) ++ [((take idx (board!!(row-1))) ++ [new] ++ drop (idx+1) (board!!(row-1)))] ++ [((take idx (board!!(row))) ++ [prev] ++ drop (idx+1) (board!!(row)))] ++ (drop (row+1) board)
 | otherwise = (take (row-1) board) ++ [((take idx (board!!(row-1))) ++ [new] ++ drop (idx+1) (board!!(row-1)))] ++ [((take idx (board!!(row))) ++ ['-'] ++ drop (idx+1) (board!!(row)))] ++ (drop (row+1) board)

moveOneDown :: [String] -> Int -> Int -> Char -> Char -> [String]
moveOneDown board row idx new prev
 | prev /= 'b' = (take row board) ++ [((take idx (board!!(row))) ++ [prev] ++ drop (idx+1) (board!!(row)))] ++ [((take idx (board!!(row+1))) ++ [new] ++ drop (idx+1) (board!!(row+1)))] ++ (drop (row+2) board)
 | otherwise = (take row board) ++ [((take idx (board!!(row))) ++ ['-'] ++ drop (idx+1) (board!!(row)))] ++ [((take idx (board!!(row+1))) ++ [new] ++ drop (idx+1) (board!!(row+1)))] ++ (drop (row+2) board)

getDirections :: [String] -> Int -> IO [String]
getDirections list x = do
    if x == 0
        then
            putStr "First direction: "
        else
            putStr "Next direction: "
    move <- getLine
    if move == ""
        then 
            return list
        else
            do
                getDirections (list ++ [move]) (x+1)


putBoard :: [String] -> IO ()
putBoard [] = return ()
putBoard (l:ls) = do
    putStrLn l
    putBoard ls