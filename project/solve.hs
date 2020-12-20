module Solve
  ( solveHelper
  , minLoopAnswer
  , minLoop
  , countLoops
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
import Solvable
import Data.Ord
import Solvable
import Helpers

-- HANDLE * next and colour current then no need for CONDITIONAL

-- This function takes the name of a text file with the map and returns an IO [String] with the solution.
-- Error check - It checks whether the map has a possible solution path or not first.
-- Parameter: String (File name)
--  Return : IO [String] (Solution)
solve :: String -> IO [String]
solve name = do
    board <- fmap lines (readFile name) 
    isSolv <- isSolvable name
    if isSolv
        then
            do
                let solutions = (solveHelper board (currBallPos board) 0 [((fst(currBallPos board)),(snd(currBallPos board)),0)] [] (length (getReachableBonuses board)))
                let shortestSol = shortest solutions
                let shortestSols = filter (\x -> ((length x) == (length shortestSol))) solutions
                return (minLoopAnswer shortestSols)
        else
            do
                return ["The path is not solvable"]

-- Simple function for counting the number of loops in a set of instructions
countLoops :: [String] -> Int
countLoops [] = 0
countLoops (m:ms) = if (take 4 m) == "Loop" then (1+ (countLoops ms)) else countLoops ms

-- Function that calls minLoop -> Simple function that iterates through a set of solutions and gives the one with the minimum number of loops
minLoopAnswer :: [[String]] -> [String]
minLoopAnswer [path] = path
minLoopAnswer (path:paths) = minLoop paths path (countLoops path)

-- Simple function that iterates through a set of solutions and gives the one with the minimum number of loops
minLoop :: [[String]] -> [String] -> Int -> [String]
minLoop [] minPath _ = minPath
minLoop (path:paths) minPath loops = if ((countLoops path) < loops) then minLoop paths path (countLoops path) else minLoop paths minPath loops

-- This function uses a DFS approach to find the Solutions to a map (Path that collects all reachable bonuses and ends at the target)
-- It saves a list of visited nodes so as to not get stuck in a loop (Node format -> (row number, column number, bonus count))
-- Parameters: Map, Current position of ball, bonus count (initially 0), visited node list (initially []), current path (initially []), total reachable bonus count
-- Returns: List of solutions that collect all reachable bonuses and end at the target
solveHelper :: [String] -> (Int, Int) -> Int -> [(Int, Int, Int)] -> [String] -> Int -> [[String]]
solveHelper board (x,y) bonus visited path totalBonuses
 | (((board !! x) !! y == 't') && (bonus /= totalBonuses)) = []
 | ((board !! x) !! y == 't') && (bonus == totalBonuses) = [path]
 | otherwise = leftSolve ++ rightSolve ++ upSolve ++ downSolve
    where   (leftBoard, (leftX, leftY), leftBonus) = left  (board, (x,y), bonus)
            (rightBoard, (rightX, rightY), rightBonus) = right  (board, (x,y), bonus)
            (upBoard, (upX, upY), upBonus) = up  (board, (x,y), bonus)
            (downBoard, (downX, downY), downBonus) = down  (board, (x,y), bonus)
            leftSolve = if ((notVisited (leftX, leftY, leftBonus)) && ((leftX, leftY)/=(x,y)))  then solveHelper leftBoard (leftX, leftY) leftBonus (visited ++ [(leftX, leftY, leftBonus)]) (parsePath leftMove) totalBonuses else []
            rightSolve = if ((notVisited (rightX, rightY, rightBonus)) && ((rightX, rightY)/=(x,y))) then solveHelper rightBoard (rightX, rightY) rightBonus (visited ++ [(rightX, rightY, rightBonus)]) (parsePath rightMove) totalBonuses else []
            upSolve = if ((notVisited (upX, upY, upBonus)) && ((upX, upY)/=(x,y))) then solveHelper upBoard (upX, upY) upBonus (visited ++ [(upX, upY, upBonus)]) (parsePath upMove) totalBonuses else []
            downSolve = if ((notVisited (downX, downY, downBonus)) && ((downX, downY)/=(x,y))) then solveHelper downBoard (downX, downY) downBonus (visited ++ [(downX, downY, downBonus)]) (parsePath downMove) totalBonuses else []
            leftMove = if ((((board !! x) !! y) `elem` ['p', 'o', 'y']) && (not (stopped last))) || ((last ++ condLeft) == lastTwo) then condLeft else ["Left"]
            rightMove = if ((((board !! x) !! y) `elem` ['p', 'o', 'y']) && (not (stopped last))) || ((last ++ condRight) == lastTwo) then condRight else ["Right"]
            upMove = if ((((board !! x) !! y) `elem` ['p', 'o', 'y']) && (not (stopped last))) || ((last ++ condUp) == lastTwo) then condUp else ["Up"]
            downMove = if ((((board !! x) !! y) `elem` ['p', 'o', 'y']) && (not (stopped last))) || ((last ++ condDown) == lastTwo) then condDown else ["Down"]
            condLeft  = if (last == ["Left"]) then [] else [("Cond{"++[((board !! x) !! y)]++"}{Left}")]
            condRight  = if (last == ["Right"]) then [] else [("Cond{"++[((board !! x) !! y)]++"}{Right}")]
            condUp  = if (last == ["Up"]) then [] else [("Cond{"++[((board !! x) !! y)]++"}{Up}")]
            condDown  = if (last == ["Down"]) then [] else [("Cond{"++[((board !! x) !! y)]++"}{Down}")]
            last = drop ((length path) -1) path
            lastToLast = take 1 $ drop ((length path)-2) path
            lastTwo = take 2 $ drop ((length path)-3) path
            notVisited (x,y,bonus) = if ((x,y,bonus) `elem` visited) then False else True
            stopped [dir]
             | dir == "Right" ||  ((drop 7 dir) == "{Right}") || ((isLoop dir) && (((getLoopMoves dir)!!1) == "Right"))  = if ((rightX, rightY)==(x,y)) then True else False
             | dir == "Left" ||  ((drop 7 dir) == "{Left}") || ((isLoop dir) && (((getLoopMoves dir)!!1) == "Left"))    = if ((leftX, leftY)==(x,y)) then True else False
             | dir == "Up" ||  ((drop 7 dir) == "{Up}") || ((isLoop dir) && (((getLoopMoves dir)!!1) == "Up"))    = if ((upX, upY)==(x,y)) then True else False
             | dir == "Down" ||  ((drop 7 dir) == "{Down}")  || ((isLoop dir) && (((getLoopMoves dir)!!1) == "Down"))   = if ((downX, downY)==(x,y)) then True else False
             | otherwise    = False
            isLoop move = if ((take 4 move) == "Loop") && ((itr move) < 5) then True else False
            getLoopMoves loop = splitString ',' (take ((length loop)-8-1) $ drop 8 loop) []
            increaseItr loop = ("Loop{" ++  (itrPlus loop) ++ "}{" ++ ((getLoopMoves loop)!!0) ++ "," ++ ((getLoopMoves loop)!!1) ++ "}")
            itrPlus loop = show ((read $ take 1 $ drop 5 loop :: Int) + 1)
            itr loop = (read $ take 1 $ drop 5 loop :: Int)
            parsePath move
             | ((length path)>0) && (isLoop (lastToLast!!0)) && ((getLoopMoves (lastToLast!!0)) == (last ++ move))   = (take ((length path)-2) path) ++ [(increaseItr (lastToLast!!0))]
             | ((length path)>2) && ((last ++ move) == lastTwo) && (not (isLoop (last!!0)))  = (take ((length path)-3) path) ++ ["Loop{2}{"++(lastTwo!!0)++","++(lastTwo!!1)++"}"]
             | otherwise    = path ++ move

-- function that gives the map state, the final position, and the bonus count after a right move
right :: ([String], (Int, Int), Int) -> ([String], (Int, Int), Int)
right  (board, (x,y), bonus)
 | (y>= ((length (board!!x))-2)) || ((board!!x)!!(y+2)) == '*' = (board, (x,y), bonus)
 | (((board!!x)!!(y+2)) `elem` ['p','o','t','y'])   = (board, (x,(y+2)), bonus)
 | (board!!x)!!(y+2) == 'b' = right ((removeBonus board x (y+2)), (x,(y+2)), (bonus+1))
 | otherwise    = right (board, (x,(y+2)),bonus)

-- function that gives the map state, the final position, and the bonus count after a left move
left :: ([String], (Int, Int), Int) -> ([String], (Int, Int), Int)
left  (board, (x,y), bonus)
 | (y < 2) || ((board!!x)!!(y-2)) == '*' = (board, (x,y), bonus)
 | (((board!!x)!!(y-2)) `elem` ['p','o','t','y'])   = (board, (x,(y-2)), bonus)
 | (board!!x)!!(y-2) == 'b' = left ((removeBonus board x (y-2)), (x,(y-2)), (bonus+1))
 | otherwise    = left (board, (x,(y-2)),bonus)

-- function that gives the map state, the final position, and the bonus count after a up move
up :: ([String], (Int, Int), Int) -> ([String], (Int, Int), Int)
up  (board, (x,y), bonus)
 | (x < 1) || ((board!!(x-1))!!y) == '*' = (board, (x,y), bonus)
 | (((board!!(x-1))!!y) `elem` ['p','o','t','y'])   = (board, ((x-1),y), bonus)
 | ((board!!(x-1))!!y) == 'b' = up ((removeBonus board (x-1) y), ((x-1),y), (bonus+1))
 | otherwise    = up (board, ((x-1),y),bonus)

-- function that gives the map state, the final position, and the bonus count after a down move
down :: ([String], (Int, Int), Int) -> ([String], (Int, Int), Int)
down  (board, (x,y), bonus)
 | (x >= ((length board)-1)) || ((board!!(x+1))!!y) == '*' = (board, (x,y), bonus)
 | (((board!!(x+1))!!y) `elem` ['p','o','t','y'])   = (board, ((x+1),y), bonus)
 | ((board!!(x+1))!!y) == 'b' = down ((removeBonus board (x+1) y), ((x+1),y), (bonus+1))
 | otherwise    = down (board, ((x+1),y),bonus)

-- removes bonus at a particular position of the board
removeBonus :: [String] -> Int -> Int -> [String]
removeBonus board x y = (take x board) ++ [((take y (board!!x)) ++ "-" ++ (drop (y+1) (board!!x)))] ++ (drop (x+1) board)