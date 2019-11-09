{-|
Module      : Simulation
Description : Simulation Module for Declaretive Programming, University of Applied Sciences Utrecht
Copyright   : (c) Berry Hijwegen, 2019
License     : GPL-3
Maintainer  : berry.hijwegen@student.hu.nl
Stability   : experimental
Portability : POSIX
-}

module Simulation (
    printSimulation,generatePlatform,stepForward
    ) where

import Position (decideDirection, getDistance, generateRandomPositions, Pos)
import Creature
import Helpers (replace, exitWithMessage, getRandomSeed)
import Data.List (delete)

-- | datatype Simulation includes all needed data to show the grid with all its information.
data Simulation = Simulation
  { _creatures      :: [Creature]           -- ^ List of creatures in simulation
  , _foods          :: [Pos]                -- ^ Positions of all current foods
  , _gridsize       :: Int                  -- ^ size of grid: _gridsize * _gridsize
  } deriving (Show)

-- | generate creatures, foods with a pseudo-random position using two seeds
generatePlatform :: Int                     -- ^ Input: Number of creatures to spawn
                 -> Int                     -- ^ Input: Number of food items to spawn 
                 -> Int                     -- ^ Input: height and width of grid 
                 -> Int                     -- ^ Input: Seed for pseudo-random numbers 
                 -> Simulation              -- ^ Output: Generated Simulation
generatePlatform n_creatures n_foods gridsize seed = Simulation {
        _creatures=generateCreatures n_creatures gridsize (getRandomSeed seed 1)  (getRandomSeed seed 2), 
        _foods=generateRandomPositions n_foods gridsize (getRandomSeed seed 3)  (getRandomSeed seed 4),
        _gridsize=gridsize
    }

-- | Retrieve positions of food items in given Simulation
getFoodPositions :: Simulation              -- ^ Input: Simulation to retrieve food items from
                 -> [Pos]                   -- ^ Output: Food positions from given Simulation
getFoodPositions (Simulation _ _foods _) = _foods

-- | Delete a given food item from given Simulation
deleteFood :: Simulation                    -- ^ Input: Simulation to delete given food item from
           -> Pos                           -- ^ Input: Food item to delete
           -> Simulation                    -- ^ Output: Simulation with given food item deleted
deleteFood s f = s {_foods = delete f $ getFoodPositions s}

-- | Find nearest Food in the Simulation given a Creature
findNearestFood :: Simulation               -- ^ Input: Simulation where Creature is in
                -> Creature                 -- ^ Input: Creature to search nearest food item for
                -> Pos                      -- ^ Output: Position of nearest food
findNearestFood s c = snd $ minimum [ (getDistance f_pos (getCreaturePos c), f_pos) | f_pos <- getFoodPositions s]

-- | Move a Creature to nearest food, replace in Simulation
moveToNearestFood :: Simulation             -- ^ Input: Simulation where Creature is in
                  -> Creature               -- ^ Input: Creature to move to nearest food item
                  -> Simulation             -- ^ Output: Given Simulation where given Creature is moved one step
moveToNearestFood s c = s {_creatures = replace c (moveCreature c $ decideDirection (getCreaturePos c) (findNearestFood s c) ) $ getCreatures s}

-- Retrieve all creatures in given Simulation
getCreatures :: Simulation                  -- ^ Input: Simulation to retrieve Creatures from
             -> [Creature]                  -- ^ Output: All creatures in the given Simulation
getCreatures (Simulation _creatures _ _) = _creatures  

-- | Retrieve the position of each Creature in given Simulation
getCreaturePositions :: Simulation          -- ^ Input: Simulation to retrieve Creature positions from
                     -> [Pos]               -- ^ Output: Position of all Creatures in given Simulation
getCreaturePositions (Simulation _creatures _ _) = map getCreaturePos _creatures  

-- | Delete Creature from given Simulation
deleteCreature :: Simulation                -- ^ Input: Simulation to delete Creature from
               -> Creature                  -- ^ Input: Creature to delete from given Simulation
               -> Simulation                -- ^ Output: Simulation with given Creature deleted
deleteCreature s c = s {_creatures = delete c $ getCreatures s}

-- | Retrieve gridsize of given Simulation
getGridSize :: Simulation                   -- ^ Input: Simulation to retrieve gridsize from
            -> Int                          -- ^ Output: Gridsize from given Simulation
getGridSize (Simulation _ _ _gridsize) = _gridsize

-- | Print Creature stats to console for given Simulation
printSimulationInfo :: Simulation           -- ^ Input: Simulation to print Creature stats for
                    -> IO ()                -- ^ Output: Printed Creature stats to console
printSimulationInfo s = mapM_ (putStrLn . getCreatureInfo) (getCreatures s)

-- | Print grid to console for given Simulation
printSimulationGrid :: Simulation           -- ^ Input: Simulation to print grid for
                    -> IO ()                -- ^ Output: Printed grid to console
printSimulationGrid s = printGrid $ generateGrid s

-- | Print grid + stats to console for given Simulation
printSimulation :: Simulation               -- ^ Input: Simulation to print grid + stats for
                -> IO [()]                  -- ^ Output: Printed grid + stats to console
printSimulation s = if not (null (getCreatures s)) && not (null (getFoodPositions s))
  then
    sequence [
      printSimulationInfo s,
      printSimulationGrid s
    ]
  else
    sequence [
      printSimulationInfo s,
      printSimulationGrid s,
      exitWithMessage $ "Simulation ended. " ++ show (length $ getCreatures s) ++ " creatures left and "  ++ show (length $ getFoodPositions s) ++ " food items left."
    ]

-- | Generates grid row. 0 = nothing, 1 = Creature, 2 = food
generateRow :: Simulation                   -- ^ Input: Simulation to generate row for
            -> Int                          -- ^ Input: Row number, needed to check if there is a Creature/item on a cell
            -> [Int]                        -- ^ Output: List of 0,1,2. 0 = nothing, 1 = Creature, 2 = food
generateRow n row_num = [ 
    if (i,row_num) `elem` getFoodPositions n 
      then 2 
    else if (i,row_num) `elem` getCreaturePositions n 
      then 1 
    else 0 
    | i <- [1..getGridSize n]
  ]

-- | Generates s nested rows of s 0's
generateGrid :: Simulation                  -- ^ Input: Simulation to generate grid for
             -> [[Int]]                     -- ^ Output: Grid (Nested list) with 0's, 1's and 2's. 0 = nothing, 1 = Creature, 2 = food.
generateGrid s = [generateRow s i | i <- [1..getGridSize s]]

-- | Convert list of numbers to list of strings
listToString :: [Int]                       -- ^ Input: List of integers
             -> [String]                    -- ^ Output: List of stringified integers
listToString = map show

-- | Convert list of numbers to a spaced string
printRow :: [Int]                           -- ^ Input: List of integers
         -> String                          -- ^ Output: All integers concatenated into one String. e.g. "0 1 0 2 0" 
printRow xs = unwords $ listToString xs

-- | Print nested list of Integers as grid
printGrid :: [[Int]]                        -- ^ Input: Nested list of integers to print as grid
          -> IO ()                          -- ^ Output: Printed grid
printGrid grid = do 
  mapM_ (putStrLn . printRow) grid
  printBorder $ length grid

-- | Print String of `l` times "- ". Functions as border for grid
printBorder :: Int                          -- ^ Input: Number of times to replicate "- "
            -> IO ()                        -- ^ Output: Printed String to console
printBorder l = putStrLn $ concat (replicate l "- ")

-- | Execute every function needed for one "step": 1. Let each Creature do one step. 2. Check if any Creature has collided with food & handle this collision. 3. Decrease HP for every Creature. 4. Remove Creatures which died from Simulation.
stepForward :: Simulation                   -- ^ Input: Simulation to do one step forward
            -> Simulation                   -- ^ Output: Given Simulation +1 step
stepForward s = removeDiedCreatures $ decreaseHp $ checkCollisions $ setOneStep s

-- | Decrease hp for each Creature
decreaseHp :: Simulation                    -- ^ Input: Simulation to decrease hp levels for each Creature
           -> Simulation                    -- ^ Output: Given Simulation with decreased hp levels for each Creature
decreaseHp s = s {_creatures = map (alterHp (-10)) $ getCreatures s}

-- | do one step in grid for each Creature
setOneStep :: Simulation                    -- ^ Input: Simulation to set one step for each Creature
           -> Simulation                    -- ^ Output: Simulation where each Creature moved one step
setOneStep s = foldl moveToNearestFood s $ getCreatures s

-- | Check for each Creature if it collided with food
checkCollisions :: Simulation               -- ^ Input: Simulation to check for collisions
                -> Simulation               -- ^ Output: Given Simulation where collisions were checked and handled (added hp, removed regarding food items)
checkCollisions s = foldl checkCreatureFoodCollision s $ getCreatures s

-- | Check if Creature is on food position
checkCreatureFoodCollision :: Simulation    -- ^ Input: Simulation where given Creature is in
                           -> Creature      -- ^ Input: Creature to check collisions for
                           -> Simulation    -- ^ Output: Given Simulation with given Creature checked, and if collided hp added and food item removed
checkCreatureFoodCollision s c = if getCreaturePos c `elem` getFoodPositions s 
  then 
    handleCreatureFoodCollision s c
  else
    s

-- | Handle Creature Food collision, add hp, remove food item.
handleCreatureFoodCollision :: Simulation   -- ^ Input: Simulation where given Creature is in
                            -> Creature     -- ^ Input: Creature to handle collision for
                            -> Simulation   -- ^ Output: Given Simulation with given Creature hp added and food item removed
handleCreatureFoodCollision s c = deleteFood (s {_creatures = replace c (alterHp 25 c) $ getCreatures s}) $ getCreaturePos c

-- | Remove all creatures with 0 or less hp
removeDiedCreatures :: Simulation           -- ^ Input: Simulation to delete died creatures from
                    -> Simulation           -- ^ Output: Given Simulation where died creatures are deleted
removeDiedCreatures s = s {_creatures = [c | c <- getCreatures s, getHp c > 0]}