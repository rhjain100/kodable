module Solve
  ( solveHelper
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

-- HANDLE * next and colour current then no need for CONDITIONAL

shortest list = minimumBy (comparing length) list

currBallPos :: [String]  -> (Int,Int)
currBallPos xs = if length positions == 1 then head positions else (-1,-1)
                    where positions = getPos xs '@'

getPos :: [String] -> Char -> [(Int, Int)]
getPos xs c = [(i,j) | (i,l) <- (zip [0..] xs), j <- elemIndices c l]

solve :: String -> IO [String]
solve name = do
    board <- fmap lines (readFile name)
    isSolv <- isSolvable name
    if isSolv
        then
            return (shortest (solveHelper board (currBallPos board) 0 [((fst(currBallPos board)),(snd(currBallPos board)),0)] [] (length (getReachableBonuses board))))
        else
            do
                return ["The path is not solvable"]

solveHelper :: [String] -> (Int, Int) -> Int -> [(Int, Int, Int)] -> [String] -> Int -> [[String]]
solveHelper board (x,y) bonus visited path totalBonuses
 | (((board !! x) !! y == 't') && (bonus /= totalBonuses)) = []
 | ((board !! x) !! y == 't') && (bonus == totalBonuses) = [path]
 | otherwise = leftSolve ++ rightSolve ++ upSolve ++ downSolve
    where   (leftBoard, (leftX, leftY), leftBonus) = left  (board, (x,y), bonus)
            (rightBoard, (rightX, rightY), rightBonus) = right  (board, (x,y), bonus)
            (upBoard, (upX, upY), upBonus) = up  (board, (x,y), bonus)
            (downBoard, (downX, downY), downBonus) = down  (board, (x,y), bonus)
            leftSolve = if ((notVisited (leftX, leftY, leftBonus)) && ((leftX, leftY)/=(x,y)))  then solveHelper leftBoard (leftX, leftY) leftBonus (visited ++ [(leftX, leftY, leftBonus)]) (path ++ leftMove) totalBonuses else []
            rightSolve = if ((notVisited (rightX, rightY, rightBonus)) && ((rightX, rightY)/=(x,y))) then solveHelper rightBoard (rightX, rightY) rightBonus (visited ++ [(rightX, rightY, rightBonus)]) (path ++ rightMove) totalBonuses else []
            upSolve = if ((notVisited (upX, upY, upBonus)) && ((upX, upY)/=(x,y))) then solveHelper upBoard (upX, upY) upBonus (visited ++ [(upX, upY, upBonus)]) (path ++ upMove) totalBonuses else []
            downSolve = if ((notVisited (downX, downY, downBonus)) && ((downX, downY)/=(x,y))) then solveHelper downBoard (downX, downY) downBonus (visited ++ [(downX, downY, downBonus)]) (path ++ downMove) totalBonuses else []
            leftMove = if (((board !! x) !! y) `elem` ['p', 'o', 'y']) then condLeft else ["Left"]
            rightMove = if (((board !! x) !! y) `elem` ['p', 'o', 'y']) then condRight else ["Right"]
            upMove = if (((board !! x) !! y) `elem` ['p', 'o', 'y']) then condUp else ["Up"]
            downMove = if (((board !! x) !! y) `elem` ['p', 'o', 'y']) then condDown else ["Down"]
            condLeft  = if (last == ["Left"]) then [] else [("Cond{"++[((board !! x) !! y)]++"}{Left}")]
            condRight  = if (last == ["Right"]) then [] else [("Cond{"++[((board !! x) !! y)]++"}{Right}")]
            condUp  = if (last == ["Up"]) then [] else [("Cond{"++[((board !! x) !! y)]++"}{Up}")]
            condDown  = if (last == ["Down"]) then [] else [("Cond{"++[((board !! x) !! y)]++"}{Down}")]
            last = drop ((length path) -1) path
            notVisited (x,y,bonus) = if ((x,y,bonus) `elem` visited) then False else True

right :: ([String], (Int, Int), Int) -> ([String], (Int, Int), Int)
right  (board, (x,y), bonus)
 | (y>= ((length (board!!x))-2)) || ((board!!x)!!(y+2)) == '*' = (board, (x,y), bonus)
 | (((board!!x)!!(y+2)) `elem` ['p','o','t','y'])   = (board, (x,(y+2)), bonus)
 | (board!!x)!!(y+2) == 'b' = right ((removeBonus board x (y+2)), (x,(y+2)), (bonus+1))
 | otherwise    = right (board, (x,(y+2)),bonus)

left :: ([String], (Int, Int), Int) -> ([String], (Int, Int), Int)
left  (board, (x,y), bonus)
 | (y < 2) || ((board!!x)!!(y-2)) == '*' = (board, (x,y), bonus)
 | (((board!!x)!!(y-2)) `elem` ['p','o','t','y'])   = (board, (x,(y-2)), bonus)
 | (board!!x)!!(y-2) == 'b' = left ((removeBonus board x (y-2)), (x,(y-2)), (bonus+1))
 | otherwise    = left (board, (x,(y-2)),bonus)

up :: ([String], (Int, Int), Int) -> ([String], (Int, Int), Int)
up  (board, (x,y), bonus)
 | (x < 1) || ((board!!(x-1))!!y) == '*' = (board, (x,y), bonus)
 | (((board!!(x-1))!!y) `elem` ['p','o','t','y'])   = (board, ((x-1),y), bonus)
 | ((board!!(x-1))!!y) == 'b' = up ((removeBonus board (x-1) y), ((x-1),y), (bonus+1))
 | otherwise    = up (board, ((x-1),y),bonus)

down :: ([String], (Int, Int), Int) -> ([String], (Int, Int), Int)
down  (board, (x,y), bonus)
 | (x >= ((length board)-1)) || ((board!!(x+1))!!y) == '*' = (board, (x,y), bonus)
 | (((board!!(x+1))!!y) `elem` ['p','o','t','y'])   = (board, ((x+1),y), bonus)
 | ((board!!(x+1))!!y) == 'b' = down ((removeBonus board (x+1) y), ((x+1),y), (bonus+1))
 | otherwise    = down (board, ((x+1),y),bonus)

removeBonus :: [String] -> Int -> Int -> [String]
removeBonus board x y = (take x board) ++ [((take y (board!!x)) ++ "-" ++ (drop (y+1) (board!!x)))] ++ (drop (x+1) board)