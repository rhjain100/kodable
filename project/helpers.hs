module Helpers
  ( shortest
  , currBallPos
  , getPos
  , putBoard
  , getBoardList
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

import Data.Ord

getBoardList :: String -> IO [String]
getBoardList name = do 
    brd <- fmap lines (readFile name)
    return brd

shortest list = minimumBy (comparing length) list

currBallPos :: [String]  -> (Int,Int)
currBallPos xs = if length positions == 1 then head positions else (-1,-1)
                    where positions = getPos xs '@'

getPos :: [String] -> Char -> [(Int, Int)]
getPos xs c = [(i,j) | (i,l) <- (zip [0..] xs), j <- elemIndices c l]

putBoard :: [String] -> IO ()
putBoard [] = return ()
putBoard (l:ls) = do
    putStrLn l
    putBoard ls