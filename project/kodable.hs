module Kodable
  ( load
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
import Play

menu :: IO ()
menu = do
    putStrLn "The available options are:"
    putStrLn "check\nsolve\nplay\nquit"
    putStrLn "Please enter one of the above options (Without spaces and capital letters)"
    putStrLn "[You have the option to enter play with 3 space \nseparated moves (directions and conditionals) to set a Function]"

-- This Function loads the map from a text file and initiates the program
load :: String -> IO ()
load name = do
    checkRead <- readAndGet name
    if (checkRead == "Error")
        then
            do putStrLn "Unable to read file (Check file name and directory again)"
        else 
            do  putStrLn "Read map successfully!"
                readXs <- fmap lines (readFile name)
                let xs = removeEndingSpaces readXs
                if (not (valBoard xs))
                    then 
                        putStrLn "INVALID BOARD"
                    else
                        do
                            putStrLn "Initial:"
                            putBoard xs
                            menu
                            loadHelper xs

-- This function handles all the possible options in the program and calls
-- the relevant functions on the basis of certain conditions.
-- The options include: check, solve, quit, play

loadHelper :: [String] -> IO ()
loadHelper xs = do
    putStr ">"
    option <- getLine
    if option == "play"
        then
            do
                if (isSolvableHelper xs (currBallPos xs) (currBallPos xs) [(currBallPos xs)]) == True
                    then
                        do
                            moves <- getDirections [] [] 0
                            play moves xs 0 '-' [] (totalBonuses xs)
                            loadHelper xs
                    else
                        do 
                            putStrLn "The map loaded is not solvable (Play not possible). Please load another map!"
                            loadHelper xs
        else
            if ((take 5 option) == "play ")
                then
                    if (isSolvableHelper xs (currBallPos xs) (currBallPos xs) [(currBallPos xs)]) == True
                        then
                            do
                                if ((length (splitString ' ' (drop 5 option) [])) == 3) && (all (\x -> ((x `elem` ["Left", "Right", "Up", "Down"]) || (validCond x))) (splitString ' ' (drop 5 option) []))
                                    then
                                        do
                                            moves <- getDirections (splitString ' ' (drop 5 option) []) [] 0
                                            play moves xs 0 '-' (splitString ' ' (drop 5 option) []) (totalBonuses xs)
                                            loadHelper xs
                                    else
                                        do 
                                            putStrLn "Please enter valid function after play (3 moves separated by space)"
                                            loadHelper xs
                        else
                            do 
                                putStrLn "The map loaded is not solvable (Play not possible). Please load another map!"
                                loadHelper xs
                else
                    if (option == "check")
                        then
                            do
                                putStrLn ("The map loaded is " ++ (issolvable xs))
                                loadHelper xs
                        else
                            if (option == "solve")
                                then
                                    do
                                        if (isSolvableHelper xs (currBallPos xs) (currBallPos xs) [(currBallPos xs)]) == True
                                            then
                                                do
                                                    let solutions = (solveHelper xs (currBallPos xs) 0 [((fst(currBallPos xs)),(snd(currBallPos xs)),0)] [] (totalBonuses xs))
                                                    let shortestSol = shortest solutions
                                                    let shortestSols = filter (\x -> ((length x) == (length shortestSol))) solutions
                                                    let solution = (functionCondense $ minLoopAnswer shortestSols)
                                                    let solMoves = fst solution
                                                    let func = if (snd solution) /= [] then (" with Function: " ++ (stringList (snd solution))) ++ "\n" else ""
                                                    putStrLn "Solution:"
                                                    putStrLn (stringList solMoves)
                                                    putStr func
                                                    loadHelper xs
                                            else
                                                do 
                                                    putStrLn "The map loaded is Not Solvable"
                                                    loadHelper xs
                                else
                                    if (option == "quit")
                                        then
                                            do putStrLn "Quitting program..."
                                        else
                                            do 
                                                putStrLn "INVALID OPTION"
                                                loadHelper xs
                                                    where   issolvable xs = if (isSolvableHelper xs (currBallPos xs) (currBallPos xs) [(currBallPos xs)]) == True then "Solvable." else "Not Solvable."
                                                            totalBonuses board = (length (getReachableBonuses board))