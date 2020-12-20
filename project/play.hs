module Play
  ( play
  , makeMove
  , continuePlay
  , moveRight
  , moveLeft
  , moveUp
  , moveDown
  , moveOneRight
  , moveOneLeft
  , moveOneUp
  , moveOneDown
  , getDirections
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


-- The play function handles all the conditional Input and Output on the basis of 
-- what the user enters as a series of moves. The play function takes the list of moves, 
-- the current board, the bonus count, previous state of ball's cell, the function set by the user, and the number of reachable
-- bonuses. Accordingly, the play function:
-- 1. recurses through the set of moves, 
-- 2. checks validity for each move,
-- 3. edits the board, 
-- 4. prints the board iteratively after each move, and 
-- 5. defines an Ending condition for the game (Win or not)
play :: [String] -> [String] -> Int -> Char -> [String] -> Int -> IO ()
play [] _ _ _ _ _ = return ()
play ["None"] _ bonus prev _ bonusCnt = do
    if prev == 't' && (bonus == bonusCnt)
        then
            putStrLn "Congratulations! You win the game! (with all reachable bonuses)"
        else
            if prev == 't'
                then
                    putStrLn ("Congratulations! You win the game! (with " ++ (show bonus) ++ " out of " ++ (show bonusCnt) ++ " bonuses")
                else
                    putStrLn "Sorry, you couldn't reach the target."
play (move:next:moves) board bonus prev func bonusCnt = do
    if ((prev == 't') && (bonus == bonusCnt)) || ((take 5 move) == "Save ")
        then
            do
                if ((take 5 move) /= "Save ")
                    then
                        do
                            putStrLn "You have already reached target with all bonuses (But with extra remaining moves)"
                            putStrLn "Congratulations! You win the game!"
                    else
                        do
                            saveBoard board (drop 5 move)
        else
            do
                if (move == "Hint") || (move == "Solve")
                    then
                        do
                            let solutions = (solveHelper board (currPos) 0 [((fst(currPos)),(snd(currPos)),0)] [] (bonusCnt - bonus))
                            let shortestSol = shortest solutions
                            let shortestSols = filter (\x -> ((length x) == (length shortestSol))) solutions
                            let finalAns = minLoopAnswer shortestSols
                            if (move == "Solve")
                                then
                                    do
                                        let solMoves = (replaceFunction finalAns func)
                                        let funct = if (solMoves) /= (finalAns) then (" with Function: " ++ (stringList func)) ++ "\n" else "\n"
                                        putStrLn "Remaining path for Solution:"
                                        putStrLn (stringList solMoves)
                                        putStr funct
                                else
                                    do
                                        
                                        let hint = if func == (take 3 finalAns) then "Function" else (finalAns!!0)
                                        putStrLn ("Hint: Try " ++ hint)
                                        continuePlay board bonus prev func bonusCnt
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
                                                        putStrLn ("Got " ++ (show (snd newBoard)) ++ "/" ++ (show bonusCnt) ++ " reachable bonuses")
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


-- This is a helper function for the play function that asks whether the
-- user wants to continue playing the game or not. If so, it calls the play function again.
-- Takes all the same parameters as play, except the list of moves.
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

-- This function is useful in changing the map state on the basis of the current move.
-- The function checks the validity of the move, moves the ball across the map, and returns
-- the new map along with the new bonus count.
-- It's parameters include - current board, current move, colour of next conditional move, 
-- bonus count, and previous state of ball's cell
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

-- Function edits map by moving ball to the right on the basis of the last state of the map.
-- Parameters: row, current position, previous cell state, bonus count, condiitional colour
-- return value: new row and new bonus count
moveRight :: String -> Int -> Char -> Int -> String -> (String, Int)
moveRight line position prev bonus color
 | (position > ((length line) - 3)) = (line, bonus)
 | [(line !! (newPos))] == color = (singleRight, bonus)
 | not ((line !! (newPos)) `elem` ['-', 'b', 'p', 'o', 'y', 't'])    = (line, bonus)
 | (line !! (newPos)) == 'b'  = moveRight singleRight (newPos) (line !! (newPos)) (bonus+1) color
 | otherwise    =  moveRight (singleRight) (newPos) (line !! (newPos)) bonus color
    where   singleRight = moveOneRight line position prev '@'
            newPos = position + 2

-- Function edits map by moving ball to the left on the basis of the last state of the map.
-- Parameters: row, current position, previous cell state, bonus count, condiitional colour
-- return value: new row and new bonus count
moveLeft :: String -> Int -> Char -> Int -> String -> (String, Int)
moveLeft line position prev bonus color
 | (position < 2)   = (line,bonus)
 | [(line !! (newPos))] == color = (singleLeft, bonus)
 | not ((line !! (newPos)) `elem` ['-', 'b', 'p', 'o', 'y', 't'])    = (line, bonus)
 | (line !! (newPos)) == 'b'   = moveLeft (singleLeft) (newPos) (line !! (newPos)) (bonus+1) color
 | otherwise   = moveLeft (singleLeft) (newPos) (line !! (newPos)) bonus color
    where   singleLeft = moveOneLeft line position prev '@'
            newPos = position - 2

-- Function edits map by moving ball upwards on the basis of the last state of the map.
-- Parameters: map, current row, current position, previous cell state, bonus count, condiitional colour
-- return value: new map and new bonus count
moveUp :: [String] -> Int -> Int -> Char -> Int -> String -> ([String], Int)
moveUp board row position prev bonus color
 | (row < 1)    = (board, bonus)
 | [((board !! (newPos)) !! position)] == color   = (singleUp, bonus)
 | not (((board !! (newPos)) !! position) `elem` ['-', 'b', 'p', 'o', 'y', 't'])    = (board, bonus)
 | ((board !! (newPos)) !! position) == 'b'   = moveUp (singleUp) (newPos) position ((board !! (newPos)) !! position) (bonus +1) color
 | otherwise   = moveUp (singleUp) (newPos) position ((board !! (newPos)) !! position) bonus color
    where   singleUp = moveOneUp board row position '@' prev
            newPos = row-1

-- Function edits map by moving ball downwards on the basis of the last state of the map.
-- Parameters: map, current row, current position, previous cell state, bonus count, condiitional colour
-- return value: new map and new bonus count
moveDown :: [String] -> Int -> Int -> Char -> Int -> String -> ([String], Int)
moveDown board row position prev bonus color
 | (row >= (length board)-1)    = (board, bonus)
 | [((board !! (newPos)) !! position)] == color   = (singleDown, bonus)
 | not (((board !! (newPos)) !! position) `elem` ['-', 'b', 'p', 'o', 'y', 't'])    = (board, bonus)
 | ((board !! (newPos)) !! position) == 'b'  = moveDown (singleDown) (newPos) position ((board !! (newPos)) !! position) (bonus+1) color
 | otherwise   = moveDown (singleDown) (newPos) position ((board !! (newPos)) !! position) bonus color
    where   singleDown = moveOneDown board row position '@' prev
            newPos = row+1

-- Helper function for moveRight, implements a single step to the right in a map
moveOneRight :: String -> Int -> Char -> Char -> String
moveOneRight l pos x y  = take pos l ++ [prev] ++ " " ++ [y] ++ drop (pos+3) l
    where prev = if x=='b' then '-' else x

-- Helper function for moveRight, implements a single step to the left in a map
moveOneLeft :: String -> Int -> Char -> Char -> String
moveOneLeft l pos x y = take (pos-2) l ++ [y] ++ " " ++ [prev] ++ drop (pos+1) l
    where prev = if x=='b' then '-' else x

-- Helper function for moveRight, implements a single step upwards in a map
moveOneUp :: [String] -> Int -> Int -> Char -> Char -> [String]
moveOneUp board row idx new prev = (take (row-1) board) ++ [((take idx (board!!(row-1))) ++ [new] ++ drop (idx+1) (board!!(row-1)))] ++ [((take idx (board!!(row))) ++ [newPrev] ++ drop (idx+1) (board!!(row)))] ++ (drop (row+1) board)
    where newPrev = if prev == 'b' then '-' else prev

-- Helper function for moveRight, implements a single step downwards in a map
moveOneDown :: [String] -> Int -> Int -> Char -> Char -> [String]
moveOneDown board row idx new prev = (take row board) ++ [((take idx (board!!(row))) ++ [newPrev] ++ drop (idx+1) (board!!(row)))] ++ [((take idx (board!!(row+1))) ++ [new] ++ drop (idx+1) (board!!(row+1)))] ++ (drop (row+2) board)
    where newPrev = if prev == 'b' then '-' else prev

-- This function initiates an IO state that iteratively takes the moves of a player.
-- The function builds a list of moves to be run on the currently loaded map.
-- It also checks the syntax of each of the moves entered. 
getDirections :: [String] -> [String] -> Int -> IO [String]
getDirections func list x = do
    if x == 0
        then
            putStr "First direction: "
        else
            putStr "Next direction: "
    move <- getLine
    if (move == "UNDO")
        then
            do getDirections func (take ((length list)-1) list) (x-1)
        else
            do
                if (not (validMove move) && move /= "") || ((move == "Function") && func == [])
                    then
                        do
                            if (move /= "Function")
                                then
                                    do
                                        putStr "Invalid Move, Enter again: "
                                        getDirections func list x
                                else
                                    do
                                        putStr "Function not predefined. Enter again: "
                                        getDirections func list x
                    else
                        if (move == "")
                            then
                                return list
                            else
                                if ((move == "Solve") || (move == "Hint")|| (take 5 move == "Save "))
                                    then
                                        return (list ++ [move])
                                    else
                                        do
                                            getDirections func (list ++ (parseMove func move)) (x+1)