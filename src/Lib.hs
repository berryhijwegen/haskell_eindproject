{-# LANGUAGE ParallelListComp #-}

module Lib
    ( generateRow, generateGrid, listToString, printRow, printGrid
    ) where

import System.Random
import Data.List

type Pos = (Int, Int)

data Simulation = Simulation
  { _creatures      :: [Creature]        -- ^ List of creatures in simulation
  , _foods          :: [Pos]             -- ^ Positions of all current foods
  } deriving (Show)

data Creature = Creature 
  { _hp             :: Int
  , _pos            :: Pos 
  } deriving (Show)



generateRow :: Int -> [Int]
generateRow s = [ j | i <- [1..s], j <- [0..0] ]

generateGrid :: Int -> [[Int]]
generateGrid s = [generateRow s | i <- [1..s]]

listToString :: [Int] -> [String]
listToString = map show

printRow :: [Int] -> String
printRow xs = unwords $ listToString xs

printGrid :: [[Int]] -> IO ()
printGrid = mapM_ (putStrLn . printRow)

randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . randomR (1,10))

generateRandomPositions :: Int -> StdGen -> StdGen -> [(Int,Int)]
generateRandomPositions n seed1 seed2 = zip (randomList n seed1) (randomList n seed2)

generateCreatures :: Int -> StdGen -> StdGen -> [Creature]
generateCreatures n seed1 seed2 = [Creature {_hp=100,_pos=pos} | i <- [1..n] | pos <- generateRandomPositions n seed1 seed2]

generatePlatform :: Int -> Int -> Simulation
generatePlatform n_creatures n_foods = Simulation {
        _creatures=generateCreatures n_creatures (mkStdGen 1)  (mkStdGen 2), 
        _foods=generateRandomPositions n_foods (mkStdGen 3)  (mkStdGen 4)
      }  