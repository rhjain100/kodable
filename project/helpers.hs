module Helpers
  ( shortest
  , currBallPos
  , getPos
  , putBoard
  , getBoardList
  , functionCondense
  , replaceFunction
  , getFunctionList
  , countFunc
  , bestFunction
  , splitString
  , removeEndingSpaces
  , valBoard
  , removeTrailingSpaces
  , validBoard
  , checkRow
  , validMove
  , validCond
  , validLoop
  , parseCond
  , parseMove
  , stringList
  , saveBoard
  , readAndGet
  , readwriteHandler
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
import Control.Exception

import Data.Ord

-- Function that checks the validity of a String move.
-- Parameter: move (String)
-- Return value: Boolean (True or False, indicating whether the move syntax is valid)
validMove :: String -> Bool
validMove move
 | move `elem` ["Left", "Right", "Up", "Down", "Hint", "Solve", "Function"]  = True
 | (take 5 move == "Save ") && (drop ((length move)-4) move == ".txt") && ((length move)>9)  = True
 | (take 4 move == "Cond") && (validCond move)  = True
 | (take 4 move == "Loop") && (validLoop move)  = True
 | otherwise    = False

-- Function that checks the validity of a conditional move.
-- Parameter: move (String)
-- Return value: Boolean (True or False, indicating whether the conditional move syntax is valid)
validCond :: String -> Bool
validCond move = if (color `elem` ["p", "o", "y"]) && (direction `elem` ["Left", "Right", "Up", "Down"]) then True else False
    where   color = (take 1 $ drop 5 move)
            direction = (take (len-8-1) $ drop 8 move)
            len = length move

-- Function that checks the validity of a Loop move.
-- Parameter: move (String)
-- Return value: Boolean (True or False, indicating whether the Loop move syntax is valid)
validLoop :: String -> Bool
validLoop move = if (((itr >= 0) && (itr <= 5)) && ((length contents) == 2) && (all (\x -> ((x `elem` moves) || (validCond x))) contents)) then True else False
    where   itr = read $ take 1 $ drop 5 move :: Int
            contents = (splitString ',' (take (len-8-1) $ drop 8 move) [])
            len = length move
            moves = ["Left", "Right", "Up", "Down"]

-- Function that parses the conditional move and gives the color and direction extracted from it
-- Parameter: move (String)
-- Return value: [String] list with first element colour and second element direction
parseCond :: String -> [String]
parseCond move
 | (length move > 4) && ((take 4 move) == "Cond")   = [(take 1 $ drop 5 move), (take (len-8-1) $ drop 8 move)]
 | otherwise    = [move]
    where   len = length move

-- Function that parses the each of the moves
-- Parameter: move (String)
-- Return value: [String] list with all parsed moves (conditionals, loops and functions require parsing)
parseMove :: [String] -> String -> [String]
parseMove func move
 | (length move > 4) && ((take 4 move) == "Cond")   = parseCond move
 | (length move > 4) && ((take 4 move) == "Loop") = concat $ map (parseCond) $ concat $ replicate itr (splitString ',' (take (len-8-1) $ drop 8 move) [])
 | move == "Function"   = concat $ map (parseCond) func
 | otherwise = [move]
    where   len = length move
            itr = read $ take 1 $ drop 5 move :: Int

-- Function that checks whether a row in a map is valid or not
checkRow :: String -> Int -> Int -> Bool
checkRow [] _ _ = True
checkRow (c:cs) pos rowLen
 | ((pos `rem` 2) == 0) && pos<=(rowLen -1)  = if c `elem` ['p','o','y','t','-','b', '@', '*'] then checkRow cs (pos+1) rowLen else False
 | otherwise    = if (c== ' ') then checkRow cs (pos+1) rowLen else False

-- function that checks whether a map is valid or not (Just checks all cells)
validBoard :: [String] -> Int -> Bool
validBoard [] _ = True
validBoard (row:rows) rowLen = if (checkRow row 0 rowLen) == True then validBoard rows rowLen else False

-- function that checks whether a map is valid or not (Just checks all cells)
valBoard :: [String] -> Bool
valBoard board = if ((validBoard board (length (board!!0))) == False) || ((currBallPos board) == (-1,-1)) || ((length (getPos board 't')) /= 1) then False else True

-- Removes ending spaces in a row so that it can be validated afterwards
removeTrailingSpaces :: String -> String
removeTrailingSpaces row = if ((last row) == " ") then removeTrailingSpaces (take ((length row)-1) row) else row
    where last row = drop ((length row)-1) row

-- Removes ending spaces in a map so that it can be validated afterwards
removeEndingSpaces :: [String] -> [String]
removeEndingSpaces [] = []
removeEndingSpaces (l:ls) = [((removeTrailingSpaces l) ++ " ")] ++ removeEndingSpaces ls 

-- Function that splits a string on the basis of the delimiter provided.
-- Parameters: Delimiter (Char), String to split, and Empty list [] (To append to)
-- Return value: List of strings after splitting
splitString :: Char -> String -> String -> [String]
splitString del [] ds = [ds]
splitString del (c:cs) ds = if c==del then (ds:splitString del cs []) else splitString del cs (ds++[c])

-- Returns a list of Strings representing each line from the file read.
-- Parameter: File name (String)
-- Return value: Map (List of strings)
getBoardList :: String -> IO [String]
getBoardList name = do 
    brd <- fmap lines (readFile name)
    return brd

-- Gives the shortest list in a list of lists
shortest list = minimumBy (comparing length) list

-- Returns the current position of the ball on the map
-- Parameters: [String] Map
-- Return value: (Int, Int) position
currBallPos :: [String]  -> (Int,Int)
currBallPos xs = if length positions == 1 then head positions else (-1,-1)
                    where positions = getPos xs '@'

-- Returns the positions of a character on the map
-- Parameters: [String] Map, [Char] item to find
-- Return value: [(Int, Int)] list of positions
getPos :: [String] -> Char -> [(Int, Int)]
getPos xs c = [(i,j) | (i,l) <- (zip [0..] xs), j <- elemIndices c l]

-- Prints the board on to the terminal
-- Parameters: [String] Map
-- Return value: IO ()
putBoard :: [String] -> IO ()
putBoard [] = return ()
putBoard (l:ls) = do
    putStrLn l
    putBoard ls

-- Function that takes a solution and condenses it using the best possible function
-- Parameters: [String] Moves
-- Return value: ([String], [String]) with the first item being the condensed solution and the second item being the best possible function
functionCondense :: [String] -> ([String], [String])
functionCondense sol = if (freq>0) then ((replaceFunction sol func), func) else (sol, [])
    where   result = if (length functionList)>0 then bestFunction (drop 1 functionList) sol (functionList!!0) (countFunc sol (functionList!!0)) else ([],0)
            functionList = getFunctionList sol
            func = fst result
            freq = snd result

-- Helper Function for functionCondense that takes a solution and condenses it using the given function
-- Parameters: [String] Moves and [String] function
-- Return value: [String] with the condensed solution
replaceFunction :: [String] -> [String] -> [String]
replaceFunction solution [] = solution
replaceFunction solution func
 | ((length solution) < 3) = solution
 | func == (take 3 solution)    = ["Function"] ++ (replaceFunction (drop 3 solution) func)
 | otherwise = (take 1 solution) ++ (replaceFunction (drop 1 solution) func)

-- get list of all possible functions from a solution
getFunctionList :: [String] -> [[String]]
getFunctionList solution
 | ((length solution) >= 3) && (all (\x -> (not (isLoop x))) (take 3 solution))   = [take 3 solution] ++ getFunctionList (drop 1 solution)
 | ((length solution) >= 3) && (not ((all (\x -> (not (isLoop x))) (take 3 solution)))) = getFunctionList (drop 1 solution)
 | otherwise    = []
    where isLoop move = if ((take 4 move) == "Loop") then True else False

-- get the most frequently occuring function in a particular set of moves
bestFunction :: [[String]] -> [String] -> [String] -> Int -> ([String], Int)
bestFunction [] _ _ 0 = ([],0)
bestFunction [] _ bestFunc count = (bestFunc, count)
bestFunction (option:options) solution bestFunc count
 | (countFunc solution option) > count  = bestFunction options solution option (countFunc solution option)
 | otherwise    = bestFunction options solution bestFunc count

-- count the frequency of a given function in a particular set of moves
countFunc :: [String] -> [String] -> Int
countFunc _ [] = 0
countFunc solution func
 | ((length solution) < 3) = 0
 | func == (take 3 solution)    = 1 + (countFunc (drop 3 solution) func)
 | otherwise = countFunc (drop 1 solution) func

-- converts a list of strings into a single space separated string
stringList :: [String] -> String
stringList [] = ""
stringList (l:ls) = l ++ " " ++ (stringList ls)

-- Function to save the current map in the middle of play, so as to continue from there
saveBoard :: [String] -> String -> IO ()
saveBoard board fileName = do 
                            if ((drop ((length fileName)-4) fileName) == ".txt" && length fileName > 4)
                                then 
                                    do 
                                        writeFile fileName (unlines board)
                                        putStrLn "Game saved succesfully! (To continue this game, quit and load [FileName(.txt)] " 
                                else 
                                    putStrLn "Invalid File Name. (It should have a '.txt' extension."

readAndGet :: String -> IO String
readAndGet name = do
    listOrExc <- catch (readFile name) (readwriteHandler)
    return listOrExc

readwriteHandler :: IOError -> IO String
readwriteHandler e = do return "Error"