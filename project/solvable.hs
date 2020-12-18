module Solvable
  ( isSolvable
  , isSolvableHelper
  , getBoardList
  , getReachableBonuses
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
import Helpers

isSolvable :: String -> IO Bool
isSolvable name =  do
    board <- getBoardList name
    if (isSolvableHelper board (currBallPos board) (currBallPos board) [(currBallPos board)]) == True
        then
            return True
        else
            return False

-- isSolvableHelper board parent current visited(initially [current])
isSolvableHelper :: [String] -> (Int,Int) -> (Int,Int) -> [(Int,Int)] -> Bool
isSolvableHelper board (a,b) (x,y) visited 
 | (board !! x) !! y == 't' = True
 | possibleMoves == [] = False
 | any (\x -> (x==True)) (map (\a -> isSolvableHelper board (x,y) a (visited ++ possibleMoves)) possibleMoves)  = True
 | otherwise    = False
    where possibleMoves = (getPossibleMoves board (a,b) (x,y) visited)

getPossibleMoves :: [String] -> (Int,Int) -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
getPossibleMoves board (a,b) (x,y) visited
 | ((board !! x) !! y) `elem` ['p', 'o', 'y', '@']   = colourMoves board (a,b) (x,y) visited
 | sameDirection /= []    = sameDirection
 | otherwise = colourMoves board (a,b) (x,y) visited
 where sameDirection = oneDirection board (a,b) (x,y)

colourMoves :: [String] -> (Int, Int) -> (Int,Int) -> [(Int, Int)] -> [(Int, Int)]
colourMoves board parent (x,y) visited = delete parent $ concat [left,right,up,down]
    where   left = if (y>=2) && ((board !! x) !! (y-2)) /= '*' && (not ((x,(y-2)) `elem` visited)) then [(x,(y-2))] else []
            right = if (y<((length (board!!x))-2)) && ((board !! x) !! (y+2)) /= '*' && (not ((x,(y+2)) `elem` visited)) then [(x,(y+2))] else []
            up = if (x>=1) && ((board !! (x-1)) !! y) /= '*' && (not (((x-1),y) `elem` visited)) then [((x-1),y)] else []
            down = if (x< ((length board)-1)) && ((board !! (x+1)) !! y) /= '*' && (not (((x+1),y) `elem` visited)) then [((x+1),y)] else []

oneDirection :: [String] -> (Int, Int) -> (Int,Int) -> [(Int, Int)]
oneDirection board parent (x,y)
 | isRight parent (x,y) = if (y<((length (board!!x))-2)) && ((board!!x)!!(y+2)) /= '*' then [(x,y+2)] else []
 | isLeft parent (x,y) = if (y>=2) && ((board!!x)!!(y-2)) /= '*' then [(x,y-2)] else []
 | isUp parent (x,y) = if (x>1) && ((board!!(x-1))!!y) /= '*' then [(x-1,y)] else []
 | isDown parent (x,y) = if (x<((length board)-1)) && ((board!!(x+1))!!y) /= '*' then [(x+1,y)] else []
 | otherwise    = []
    where   isRight (a,b) (x,y) = if ((y-2) == b) then True else False
            isLeft (a,b) (x,y) = if ((y+2) == b) then True else False
            isUp (a,b) (x,y) = if ((x+1) == a) then True else False
            isDown (a,b) (x,y) = if ((x-1) == a) then True else False

getReachableBonuses :: [String] -> [(Int, Int)]
getReachableBonuses board = reachableBonuses board (getPos board 'b')

reachableBonuses :: [String] -> [(Int, Int)] -> [(Int, Int)]
reachableBonuses _ [] = []
reachableBonuses board (b:bs) = if  (isSolvableHelper newBoard (currBallPos newBoard) (currBallPos newBoard) [(currBallPos newBoard)]) == True then (b : (reachableBonuses board bs)) else reachableBonuses board bs
    where   newBoard = replaceCell noTBoard b 't'
            noTBoard = replaceCell board targetPos '-'
            targetPos = head (getPos board 't')

replaceCell :: [String] -> (Int, Int) -> Char -> [String]
replaceCell board (x,y) c = (take x board) ++ [((take y (board!!x)) ++ [c] ++ (drop (y+1) (board!!x)))] ++ (drop (x+1) board)