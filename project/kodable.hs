module Kodable
  ( load
  , play
  , makeMove
  , moveRight
  , moveLeft
  , moveUp
  , moveDown
  , moveOneRight
  , moveOneLeft
  , moveOneUp
  , moveOneDown
  , parseMove
  , getDirections
  , splitString
  ) where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
-- import Data.Text.Conversions as Text

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Char
import Data.List
import Text.Printf

import Solve
import Helpers
import Solvable

checkRow :: String -> Int -> Int -> Bool
checkRow [] _ _ = True
checkRow (c:cs) pos rowLen
 | ((pos `rem` 2) == 0) && pos<=(rowLen -1)  = if c `elem` ['p','o','y','t','-','b', '@', '*'] then checkRow cs (pos+1) rowLen else False
 | otherwise    = if (c== ' ') then checkRow cs (pos+1) rowLen else False

validBoard :: [String] -> Int -> Bool
validBoard [] _ = True
validBoard (row:rows) rowLen = if (checkRow row 0 rowLen) == True then validBoard rows rowLen else False

valBoard :: [String] -> Bool
valBoard board = validBoard board (length (board!!0))

load :: String -> IO ()
load name = do
    xs <- fmap lines (readFile name)
    putStrLn "Read map successfully!"
    if (not (valBoard xs))
        then 
            putStrLn "INVALID BOARD"
        else
            do
                putStrLn "Initial:"
                putBoard xs
                loadHelper xs

loadHelper :: [String] -> IO ()
loadHelper xs = do
    putStr ">"
    option <- getLine
    if option == "play"
        then
            do
                if (isSolvableHelper xs (currBallPos xs) (currBallPos xs) [(currBallPos xs)]) == True
                    then
                        do
                            moves <- getDirections [] [] 0
                            play moves xs 0 '-' [] (totalBonuses xs)
                            loadHelper xs
                    else
                        do 
                            putStrLn "The map loaded is not solvable (Play not possible). Please load another map!"
                            loadHelper xs
        else
            if ((take 5 option) == "play ")
                then
                    if (isSolvableHelper xs (currBallPos xs) (currBallPos xs) [(currBallPos xs)]) == True
                        then
                            do
                                if (length (splitString ' ' (drop 5 option) [])) == 3
                                    then
                                        do
                                            moves <- getDirections (splitString ' ' (drop 5 option) []) [] 0
                                            play moves xs 0 '-' (splitString ' ' (drop 5 option) []) (totalBonuses xs)
                                            loadHelper xs
                                    else
                                        do 
                                            putStrLn "Please enter valid function after play (3 moves separated by space)"
                                            loadHelper xs
                        else
                            do 
                                putStrLn "The map loaded is not solvable (Play not possible). Please load another map!"
                                loadHelper xs
                else
                    if (option == "check")
                        then
                            do
                                putStrLn ("The map loaded is " ++ (issolvable xs))
                                loadHelper xs
                        else
                            if (option == "solve")
                                then
                                    do
                                        if (isSolvableHelper xs (currBallPos xs) (currBallPos xs) [(currBallPos xs)]) == True
                                            then
                                                do
                                                    putStrLn "Solution:"
                                                    putStrLn (show (shortest (solveHelper xs (currBallPos xs) 0 [((fst(currBallPos xs)),(snd(currBallPos xs)),0)] [] (totalBonuses xs))))
                                                    loadHelper xs
                                            else
                                                do 
                                                    putStrLn "The map loaded is Not Solvable"
                                                    loadHelper xs
                                else
                                    if (option == "quit")
                                        then
                                            do putStrLn "Quitting game..."
                                        else
                                            do 
                                                putStrLn "INVALID OPTION"
                                                loadHelper xs
                                                    where   issolvable xs = if (isSolvableHelper xs (currBallPos xs) (currBallPos xs) [(currBallPos xs)]) == True then "Solvable." else "Not Solvable."
                                                            totalBonuses board = (length (getReachableBonuses board))


play :: [String] -> [String] -> Int -> Char -> [String] -> Int -> IO ()
play [] _ _ _ _ _ = return ()
play ["None"] _ bonus prev _ bonusCnt = do
    if prev == 't' && (bonus == bonusCnt)
        then
            putStrLn "Congratulations! You win the game! (with all bonuses)"
        else
            if prev == 't'
                then
                    putStrLn ("Congratulations! You win the game! (with " ++ (show bonus) ++ " out of " ++ (show bonusCnt) ++ " bonuses")
                else
                    putStrLn "Sorry, you couldn't reach the target."
play (move:next:moves) board bonus prev func bonusCnt = do
    putStrLn ""
    if (((board !! (fst currPos)) !! (snd currPos)) == 't') && (bonus == bonusCnt)
        then
            do
                putStrLn "You have already reached target with all bonuses (But with extra remaining moves)"
                putStrLn "Congratulations! You win the game!"
        else
            do
                if (move == "Hint") || (move == "Solve")
                    then
                        do
                            if (move == "Solve")
                                then
                                    do
                                        if (isSolvableHelper board (currPos) (currPos) [(currPos)]) == True
                                            then
                                                do
                                                    putStrLn "Remaining path for Solution:"
                                                    putStrLn (show (shortest (solveHelper board (currPos) 0 [((fst(currPos)),(snd(currPos)),0)] [] (bonusCnt - bonus))))
                                                    putStrLn ""
                                            else
                                                do putStrLn "There is no solution from the path you have taken"
                                else
                                    do
                                        if (isSolvableHelper board (currPos) (currPos) [(currPos)]) == True
                                            then
                                                do
                                                    putStrLn ("Hint: " ++ ((shortest (solveHelper board (currPos) 0 [((fst(currPos)),(snd(currPos)),0)] [] (bonusCnt - bonus)))!!0))
                                                    putStrLn ""
                                                    continuePlay board bonus prev func bonusCnt
                                            else
                                                do putStrLn "There is no solution from the path you have taken"
                    else
                        do
                            if ((fst newBoard) == []) || (((length next) == 1) && not (next `elem` ["p","o","y"]))
                                then
                                    if (length move) == 1
                                        then
                                            putStrLn ("INVALID MOVE Cond{" ++ move ++ "}{" ++ next ++ "}")
                                        else
                                            if (((length next) == 1) && not (next `elem` ["p","o","y"]))
                                                then
                                                    putStrLn ("INVALID COLOUR " ++ next)
                                                else
                                                    putStrLn ("INVALID MOVE " ++ move)
                                else
                                    if (fst newBoard) == board
                                        then
                                            do
                                                putStrLn ("Sorry, error: cannot move " ++ move)
                                                putStrLn "Your current board:"
                                                putBoard board
                                        else
                                            do
                                                putBoard (fst newBoard)
                                                if (snd newBoard) > bonus
                                                    then
                                                        putStrLn ("Got " ++ (show ((snd newBoard)-bonus)) ++ " bonus(es) [" ++ (show (snd newBoard)) ++ " out of " ++ (show bonusCnt) ++ " reachable bonuses]")
                                                    else
                                                        putStr ""
                                                putStrLn ""
                                                if (length next == 1)
                                                    then
                                                        if [newPrev] /= next
                                                            then
                                                                putStrLn ("Condition for color " ++ next ++ " never met")
                                                            else
                                                                play moves (fst newBoard) (snd newBoard) (newPrev) func bonusCnt
                                                    else
                                                        play (next:moves) (fst newBoard) (snd newBoard) (newPrev) func bonusCnt
                                                            where   newBoard = if (length next == 1) then makeMove board move next bonus prev else makeMove board move "None" bonus prev
                                                                    newPosX = fst(currBallPos (fst newBoard))
                                                                    newPosY = snd(currBallPos (fst newBoard))
                                                                    newPrev = if ((board !! newPosX) !! newPosY) == 'b' then '-' else ((board !! newPosX) !! newPosY)
                                                                    currPos = currBallPos board
play (move:moves) board bonus prev func bonusCnt = play (move:"None":moves) board bonus prev func bonusCnt

continuePlay :: [String] -> Int -> Char -> [String] -> Int -> IO ()
continuePlay board bonus prev func bonusCnt = do
    putStr "Continue Playing? (Y/N): "
    op <- getLine
    if (op == "Y") || (op == "y")
        then
            do
                moves <- getDirections func [] 1
                play moves board bonus prev func bonusCnt
        else
            if (op == "N") || (op == "n")
                then
                    do
                        putStrLn "Ending board:"
                        putBoard board
                else
                    do 
                        putStr "Invalid Option. "
                        continuePlay board bonus prev func bonusCnt

makeMove :: [String] -> String -> String -> Int -> Char -> ([String], Int)
makeMove board move color bonus prev
 | move == "Right"  = ((take row board ++ [fst(rightRow)] ++ drop (row+1) board), (snd rightRow))
 | move == "Left"   = ((take row board ++ [fst(leftRow)] ++ drop (row+1) board), (snd leftRow))
 | move == "Up" = moveUp board row idx prev bonus color
 | move == "Down" = moveDown board row idx prev bonus color
 | otherwise    = ([],-1)
    where   rightRow = moveRight (board !! row) idx prev bonus color
            leftRow = moveLeft (board !! row) idx prev bonus color
            row = (fst (currBallPos board))
            idx = (snd (currBallPos board))

moveRight :: String -> Int -> Char -> Int -> String -> (String, Int)
moveRight line position prev bonus color
 | (position > ((length line) - 3)) = (line, bonus)
 | [(line !! (newPos))] == color = (singleRight, bonus)
 | not ((line !! (newPos)) `elem` ['-', 'b', 'p', 'o', 'y', 't'])    = (line, bonus)
 | (line !! (newPos)) == 'b'  = moveRight singleRight (newPos) (line !! (newPos)) (bonus+1) color
 | otherwise    =  moveRight (singleRight) (newPos) (line !! (newPos)) bonus color
    where   singleRight = moveOneRight line position prev '@'
            newPos = position + 2

moveLeft :: String -> Int -> Char -> Int -> String -> (String, Int)
moveLeft line position prev bonus color
 | (position < 2)   = (line,bonus)
 | [(line !! (newPos))] == color = (singleLeft, bonus)
 | not ((line !! (newPos)) `elem` ['-', 'b', 'p', 'o', 'y', 't'])    = (line, bonus)
 | (line !! (newPos)) == 'b'   = moveLeft (singleLeft) (newPos) (line !! (newPos)) (bonus+1) color
 | otherwise   = moveLeft (singleLeft) (newPos) (line !! (newPos)) bonus color
    where   singleLeft = moveOneLeft line position prev '@'
            newPos = position - 2

moveUp :: [String] -> Int -> Int -> Char -> Int -> String -> ([String], Int)
moveUp board row position prev bonus color
 | (row < 1)    = (board, bonus)
 | [((board !! (newPos)) !! position)] == color   = (singleUp, bonus)
 | not (((board !! (newPos)) !! position) `elem` ['-', 'b', 'p', 'o', 'y', 't'])    = (board, bonus)
 | ((board !! (newPos)) !! position) == 'b'   = moveUp (singleUp) (newPos) position ((board !! (newPos)) !! position) (bonus +1) color
 | otherwise   = moveUp (singleUp) (newPos) position ((board !! (newPos)) !! position) bonus color
    where   singleUp = moveOneUp board row position '@' prev
            newPos = row-1

moveDown :: [String] -> Int -> Int -> Char -> Int -> String -> ([String], Int)
moveDown board row position prev bonus color
 | (row >= (length board)-1)    = (board, bonus)
 | [((board !! (newPos)) !! position)] == color   = (singleDown, bonus)
 | not (((board !! (newPos)) !! position) `elem` ['-', 'b', 'p', 'o', 'y', 't'])    = (board, bonus)
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

getDirections :: [String] -> [String] -> Int -> IO [String]
getDirections func list x = do
    if x == 0
        then
            putStr "First direction: "
        else
            putStr "Next direction: "
    move <- getLine
    if (move == "")
        then
            return list
        else
            if (move == "Solve") || (move == "Hint")
                then
                    return (list ++ [move])
                else
                    do
                        getDirections func (list ++ (parseMove func move)) (x+1)

parseCond :: String -> [String]
parseCond move
 | (length move > 4) && ((take 4 move) == "Cond")   = [(take 1 $ drop 5 move), (take (len-8-1) $ drop 8 move)]
 | otherwise    = [move]
    where   len = length move

parseMove :: [String] -> String -> [String]
parseMove func move
 | (length move > 4) && ((take 4 move) == "Cond")   = parseCond move
 | (length move > 4) && ((take 4 move) == "Loop") = concat $ map (parseCond) $ concat $ replicate itr (splitString ',' (take (len-8-1) $ drop 8 move) [])
 | move == "Function"   = concat $ map (parseCond) func
 | otherwise = [move]
    where   len = length move
            itr = read $ take 1 $ drop 5 move :: Int

splitString :: Char -> String -> String -> [String]
splitString del [] ds = [ds]
splitString del (c:cs) ds = if c==del then (ds:splitString del cs []) else splitString del cs (ds++[c])