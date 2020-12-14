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
    putBoard (map (Text.unpack) xs)

currBallPos :: [String]  -> (Int,Int)
currBallPos xs = if length positions == 1 then head positions else (-1,-1)
                    where positions = getPos xs '@'

getPos :: [String] -> Char -> [(Int, Int)]
getPos xs c = [(i,j) | (i,l) <- (zip [0..] xs), j <- elemIndices c l]

makeMove :: [String] -> String -> [String]
makeMove board move
 | move == "Right"  = take row board ++ [moveRight (board !! row) idx '-'] ++ drop (row+1) board
 | move == "Left"   = take row board ++ [moveLeft (board !! row) idx '-'] ++ drop (row+1) board
 | move == "Up" = moveUp board row idx '-'
 | move == "Down" = moveDown board row idx '-'
 | otherwise    = error "Move invalid"
    where   row = (fst (currBallPos board))
            idx = (snd (currBallPos board))

moveRight :: String -> Int -> Char -> String
moveRight line position prev
 | (position > ((length line) - 3)) = line
 | (line !! (position + 2)) /= '-'    = line
 | position < (length line)   = moveRight (moveOneRight line position prev '@') (position + 2) (line !! (position +2))
 | otherwise    = line

moveLeft :: String -> Int -> Char -> String
moveLeft line position prev
 | (position < 2)   = line
 | (line !! (position - 2)) /= '-'    = line
 | position < (length line)   = moveLeft (moveOneLeft line position prev '@') (position - 2) (line !! (position - 2))
 | otherwise    = line      

moveUp :: [String] -> Int -> Int -> Char -> [String]
moveUp board row position prev
 | (row < 1)    = board
 | ((board !! (row-1)) !! position) /= '-'    = board
 | otherwise   = moveUp (moveOneUp board row position '@' prev) (row-1) position ((board !! (row-1)) !! position)   

moveDown :: [String] -> Int -> Int -> Char -> [String]
moveDown board row position prev
 | (row >= (length board)-1)    = board
 | ((board !! (row+1)) !! position) /= '-'    = board
 | otherwise   = moveDown (moveOneDown board row position '@' prev) (row+1) position ((board !! (row+1)) !! position)   

moveOneRight :: String -> Int -> Char -> Char -> String
moveOneRight l pos x y = take pos l ++ [x] ++ " " ++ [y] ++ drop (pos+3) l

moveOneLeft :: String -> Int -> Char -> Char -> String
moveOneLeft l pos x y = take (pos-2) l ++ [y] ++ " " ++ [x] ++ drop (pos+1) l

moveOneUp :: [String] -> Int -> Int -> Char -> Char -> [String]
moveOneUp board row idx new prev = (take (row-1) board) ++ [((take idx (board!!(row-1))) ++ [new] ++ drop (idx+1) (board!!(row-1)))] ++ [((take idx (board!!(row))) ++ [prev] ++ drop (idx+1) (board!!(row)))] ++ (drop (row+1) board)

moveOneDown :: [String] -> Int -> Int -> Char -> Char -> [String]
moveOneDown board row idx new prev = (take row board) ++ [((take idx (board!!(row))) ++ [prev] ++ drop (idx+1) (board!!(row)))] ++ [((take idx (board!!(row+1))) ++ [new] ++ drop (idx+1) (board!!(row+1)))] ++ (drop (row+2) board)

getDirection :: Int -> IO ()
getDirection x = do
    if x == 0
        then
            putStr "First direction: "
        else
            putStr "Next direction: "
    move <- getLine
    if move == ""
        then 
            return ()
        else
            do
                putStrLn move
                getDirection (x+1)
    

putBoard :: [String] -> IO ()
putBoard [] = return ()
putBoard (l:ls) = do
    putStrLn l
    putBoard ls