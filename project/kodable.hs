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

-- getCurrPos :: String -> IO()
-- getCurrPos name = do
--     xs <- fmap Text.lines (Text.readFile name)
--     putStr (show (getCurrPos (map (Text.unpack) xs)))

-- currPos :: [String] -> (Int,Int) -> (Int,Int)
-- currPos xs (i,j)
--  | ((xs!!i)!!j) == '@'  = (i,j)
--  | j < (length (xs!!i) - 2)   = currPos xs (i,j+2)
--  | i < length xs    = currPos xs (i+1,0)
--  | otherwise    = (-1,-1)

currBallPos :: [String]  -> (Int,Int)
currBallPos xs = if length positions == 1 then head positions else (-1,-1)
                    where positions = getPos xs '@'

getPos :: [String] -> Char -> [(Int, Int)]
getPos xs c = [(i,j) | (i,l) <- (zip [0..] xs), j <- elemIndices c l]

moveOneRight :: String -> Int -> Char -> Char -> String
moveOneRight l pos x y = take pos l ++ [x] ++ " " ++ [y] ++ drop (pos+3) l

moveOneLeft :: String -> Int -> Char -> Char -> String
moveOneLeft l pos x y = take (pos-2) l ++ [y] ++ " " ++ [x] ++ drop (pos+1) l

makeMove :: [String] -> String -> [String]
makeMove board move
 | move == "Right"  = take row board ++ [moveRight (board !! row) idx '-'] ++ drop (row+1) board
 | move == "Left"   = take row board ++ [moveLeft (board !! row) idx '-'] ++ drop (row+1) board
 | otherwise    = error "Move invalid"
    where   row = (fst (currBallPos board))
            idx = (snd (currBallPos board))
--  | move == "Left"   = moveLeft [String]
--  | move == "Up"  = moveUp [String]
--  | move == "Down"   = moveDown [String]

moveRight :: String -> Int -> Char -> String
moveRight line position prev
 | (position > ((length line) - 2)) || (line !! (position + 2)) /= '-'    = line
 | position < (length line)   = moveRight (moveOneRight line position prev '@') (position + 2) (line !! (position +2))
 | otherwise    = line

moveLeft :: String -> Int -> Char -> String
moveLeft line position prev
 | (position < 2) || (line !! (position - 2)) /= '-'    = line
 | position < (length line)   = moveLeft (moveOneLeft line position prev '@') (position - 2) (line !! (position - 2))
 | otherwise    = line        


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
            
        
