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

getBoardList :: String -> IO [String]
getBoardList name = do 
    brd <- fmap lines (readFile name)
    return brd

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