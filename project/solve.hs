import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
-- import Data.Text.Conversions as Text

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Char
import Data.List

import Text.Printf

import Kodable
import Solvable
import Data.Ord

shortest list = minimumBy (comparing length) list

solve :: [String] -> (Int, Int) -> Int -> [(Int, Int, Int)] -> [String] -> [[String]]
solve board (x,y) bonus visited path
 | (((board !! x) !! y == 't') && (bonus /= 3)) = []
 | ((board !! x) !! y == 't') && (bonus == 3) = [path]
 | otherwise = leftSolve ++ rightSolve ++ upSolve ++ downSolve
    where   (leftBoard, (leftX, leftY), leftBonus) = left  (board, (x,y), bonus)
            (rightBoard, (rightX, rightY), rightBonus) = right  (board, (x,y), bonus)
            (upBoard, (upX, upY), upBonus) = up  (board, (x,y), bonus)
            (downBoard, (downX, downY), downBonus) = down  (board, (x,y), bonus)
            leftSolve = if ((notVisited (leftX, leftY, leftBonus)) && ((leftX, leftY)/=(x,y)))  then solve leftBoard (leftX, leftY) leftBonus (visited ++ [(leftX, leftY, leftBonus)]) (path ++ [leftMove]) else []
            rightSolve = if ((notVisited (rightX, rightY, rightBonus)) && ((rightX, rightY)/=(x,y))) then solve rightBoard (rightX, rightY) rightBonus (visited ++ [(rightX, rightY, rightBonus)]) (path ++ [rightMove]) else []
            upSolve = if ((notVisited (upX, upY, upBonus)) && ((upX, upY)/=(x,y))) then solve upBoard (upX, upY) upBonus (visited ++ [(upX, upY, upBonus)]) (path ++ [upMove]) else []
            downSolve = if ((notVisited (downX, downY, downBonus)) && ((downX, downY)/=(x,y))) then solve downBoard (downX, downY) downBonus (visited ++ [(downX, downY, downBonus)]) (path ++ [downMove]) else []
            leftMove = if (((board !! x) !! y) `elem` ['p', 'o', 'y']) then ("Cond{"++[((board !! x) !! y)]++"}{Left}") else "Left"
            rightMove = if (((board !! x) !! y) `elem` ['p', 'o', 'y']) then ("Cond{"++[((board !! x) !! y)]++"}{Right}") else "Right"
            upMove = if (((board !! x) !! y) `elem` ['p', 'o', 'y']) then ("Cond{"++[((board !! x) !! y)]++"}{Up}") else "Up"
            downMove = if (((board !! x) !! y) `elem` ['p', 'o', 'y']) then ("Cond{"++[((board !! x) !! y)]++"}{Down}") else "Down"
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