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
                play moves (map (Text.unpack) xs) 0 '-'
    else
        putStrLn "INVALID"

currBallPos :: [String]  -> (Int,Int)
currBallPos xs = if length positions == 1 then head positions else (-1,-1)
                    where positions = getPos xs '@'

play :: [String] -> [String] -> Int -> Char -> IO ()
play ["None"] _ _ _ = return ()
play (move:next:moves) board bonus prev = do
    if (fst newBoard) == board
        then
            putStrLn ("Sorry, error: cannot move " ++ move)
        else
            do
                putBoard (fst newBoard)
                if (snd newBoard) > bonus
                    then
                        putStrLn ("Got " ++ (show ((snd newBoard)-bonus)) ++ " bonus(es)")
                    else
                        putStr ""
                putStrLn ""
                if (length next == 1)
                    then
                        if [((board !! newPosX) !! newPosY)] /= next
                            then
                                putStrLn ("Condition for color " ++ next ++ " never Met")
                            else
                                play moves (fst newBoard) (snd newBoard) ((board !! newPosX) !! newPosY)
                    else
                        play (next:moves) (fst newBoard) (snd newBoard) ((board !! newPosX) !! newPosY)
                    where   newBoard = if (length next == 1) then makeMove board move next bonus prev else makeMove board move "None" bonus prev
                            newPosX = fst(currBallPos (fst newBoard))
                            newPosY = snd(currBallPos (fst newBoard))
play (move:moves) board bonus prev = play (move:"None":moves) board bonus prev

getPos :: [String] -> Char -> [(Int, Int)]
getPos xs c = [(i,j) | (i,l) <- (zip [0..] xs), j <- elemIndices c l]

makeMove :: [String] -> String -> String -> Int -> Char -> ([String], Int)
makeMove board move color bonus prev
 | move == "Right"  = ((take row board ++ [fst(rightRow)] ++ drop (row+1) board), (snd rightRow))
 | move == "Left"   = ((take row board ++ [fst(leftRow)] ++ drop (row+1) board), (snd leftRow))
 | move == "Up" = moveUp board row idx prev bonus color
 | move == "Down" = moveDown board row idx prev bonus color
 | otherwise    = error move
    where   rightRow = moveRight (board !! row) idx prev bonus color
            leftRow = moveLeft (board !! row) idx prev bonus color
            row = (fst (currBallPos board))
            idx = (snd (currBallPos board))

moveRight :: String -> Int -> Char -> Int -> String -> (String, Int)
moveRight line position prev bonus color
 | (position > ((length line) - 3)) = (line, bonus)
 | [(line !! (newPos))] == color = (singleRight, bonus)
 | not ((line !! (newPos)) `elem` ['-', 'b', 'p', 'o', 'y'])    = (line, bonus)
 | (line !! (newPos)) == 'b'  = moveRight singleRight (newPos) (line !! (newPos)) (bonus+1) color
 | otherwise    =  moveRight (singleRight) (newPos) (line !! (newPos)) bonus color
    where   singleRight = moveOneRight line position prev '@'
            newPos = position + 2

moveLeft :: String -> Int -> Char -> Int -> String -> (String, Int)
moveLeft line position prev bonus color
 | (position < 2)   = (line,bonus)
 | [(line !! (newPos))] == color = (singleLeft, bonus)
 | not ((line !! (newPos)) `elem` ['-', 'b', 'p', 'o', 'y'])    = (line, bonus)
 | (line !! (newPos)) == 'b'   = moveLeft (singleLeft) (newPos) (line !! (newPos)) (bonus+1) color
 | otherwise   = moveLeft (singleLeft) (newPos) (line !! (newPos)) bonus color
    where   singleLeft = moveOneLeft line position prev '@'
            newPos = position - 2

moveUp :: [String] -> Int -> Int -> Char -> Int -> String -> ([String], Int)
moveUp board row position prev bonus color
 | (row < 1)    = (board, bonus)
 | [((board !! (newPos)) !! position)] == color   = (singleUp, bonus)
 | not (((board !! (newPos)) !! position) `elem` ['-', 'b', 'p', 'o', 'y'])    = (board, bonus)
 | ((board !! (newPos)) !! position) == 'b'   = moveUp (singleUp) (newPos) position ((board !! (newPos)) !! position) (bonus +1) color
 | otherwise   = moveUp (singleUp) (newPos) position ((board !! (newPos)) !! position) bonus color
    where   singleUp = moveOneUp board row position '@' prev
            newPos = row-1

moveDown :: [String] -> Int -> Int -> Char -> Int -> String -> ([String], Int)
moveDown board row position prev bonus color
 | (row >= (length board)-1)    = (board, bonus)
 | [((board !! (newPos)) !! position)] == color   = (singleDown, bonus)
 | not (((board !! (newPos)) !! position) `elem` ['-', 'b', 'p', 'o', 'y'])    = (board, bonus)
 | ((board !! (newPos)) !! position) == 'b'  = moveDown (singleDown) (newPos) position ((board !! (newPos)) !! position) (bonus+1) color
 | otherwise   = moveDown (singleDown) (newPos) position ((board !! (newPos)) !! position) bonus color
    where   singleDown = moveOneDown board row position '@' prev
            newPos = row+1

moveOneRight :: String -> Int -> Char -> Char -> String
moveOneRight l pos x y  = take pos l ++ [prev] ++ " " ++ [y] ++ drop (pos+3) l
    where prev = if x=='b' then '-' else x

moveOneLeft :: String -> Int -> Char -> Char -> String
moveOneLeft l pos x y = take (pos-2) l ++ [y] ++ " " ++ [prev] ++ drop (pos+1) l
    where prev = if x=='b' then '-' else x

moveOneUp :: [String] -> Int -> Int -> Char -> Char -> [String]
moveOneUp board row idx new prev = (take (row-1) board) ++ [((take idx (board!!(row-1))) ++ [new] ++ drop (idx+1) (board!!(row-1)))] ++ [((take idx (board!!(row))) ++ [newPrev] ++ drop (idx+1) (board!!(row)))] ++ (drop (row+1) board)
    where newPrev = if prev == 'b' then '-' else prev

moveOneDown :: [String] -> Int -> Int -> Char -> Char -> [String]
moveOneDown board row idx new prev = (take row board) ++ [((take idx (board!!(row))) ++ [newPrev] ++ drop (idx+1) (board!!(row)))] ++ [((take idx (board!!(row+1))) ++ [new] ++ drop (idx+1) (board!!(row+1)))] ++ (drop (row+2) board)
    where newPrev = if prev == 'b' then '-' else prev

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
                getDirections (list ++ (parseConditional move)) (x+1)

parseConditional :: String -> [String]
parseConditional move
 | (length move > 4) && ((take 4 move) == "Cond")   = [(take 1 $ drop 5 move), (take (len-8-1) $ drop 8 move)]
 | otherwise = [move]
    where len = length move


putBoard :: [String] -> IO ()
putBoard [] = return ()
putBoard (l:ls) = do
    putStrLn l
    putBoard ls