{-# LANGUAGE ParallelListComp #-}

module SimulationModule
    ( generatePlatform, stepForward
    ) where

import System.Random
import Data.List

type Pos = (Int, Int)

data Simulation = Simulation
  { _creatures      :: [Creature]        -- ^ List of creatures in simulation
  , _foods          :: [Pos]             -- ^ Positions of all current foods
  , _gridsize       :: Int               -- ^ size of grid: _gridsize * _gridsize
  } deriving (Show)

data Direction = Up' | Down' | Left' | Right'

getFoodPositions :: Simulation -> [Pos]
getFoodPositions (Simulation _ _foods _) = _foods

getCreatures :: Simulation -> [Creature]
getCreatures (Simulation _creatures _ _) = _creatures  

getCreaturePositions :: Simulation -> [Pos]
getCreaturePositions (Simulation _creatures _ _) = map getCreaturePos _creatures  

getGridSize :: Simulation -> Int
getGridSize (Simulation _ _ _gridsize) = _gridsize  


data Creature = Creature 
  { _hp             :: Int
  , _pos            :: Pos 
  } deriving (Show, Eq, Ord)

-- | Returns positions of given Creature
getCreaturePos :: Creature -> Pos
getCreaturePos (Creature _ _pos) = _pos



-- | Generates row of s 0's 
generateRow :: Simulation -> Int -> [Int]
generateRow s row_num = [ 
    if (row_num,i) `elem` getFoodPositions s 
      then 2 
    else if (row_num,i) `elem` getCreaturePositions s 
      then 1 
    else 0 
    | i <- [1..getGridSize s]
  ]

-- | Generates s nested rows of s 0's
generateGrid :: Simulation -> [[Int]]
generateGrid s = [generateRow s i | i <- [1..getGridSize s]]

-- | Convert list of numbers to list of strings
listToString :: [Int] -> [String]
listToString = map show

-- | Convert list of numbers to a spaced string
printRow :: [Int] -> String
printRow xs = unwords $ listToString xs

-- | Print given grid
printGrid :: [[Int]] -> IO ()
printGrid = mapM_ (putStrLn . printRow)


printSimulation :: Simulation -> IO ()
printSimulation s = printGrid $ generateGrid s

-- | Create pseudo-random list of n numbers using a seed
randomList :: Int -> Int -> StdGen -> [Int]
randomList n max = take n . unfoldr (Just . randomR (1,max))

-- | Create list of pseudo-random positions using two seeds
generateRandomPositions :: Int -> Int -> StdGen -> StdGen -> [Pos]
generateRandomPositions n max seed1 seed2 = zip (randomList n max seed1) (randomList n max seed2)

-- | generate creatures with a pseudo-random position using two seeds
generateCreatures :: Int -> Int -> StdGen -> StdGen -> [Creature]
generateCreatures n max seed1 seed2 = [Creature {_hp=100,_pos=pos} | i <- [1..n] | pos <- generateRandomPositions n max seed1 seed2]

-- | generate creatures, foods with a pseudo-random position using two seeds
generatePlatform :: Int -> Int -> Int -> Simulation
generatePlatform n_creatures n_foods gridsize = Simulation {
        _creatures=generateCreatures n_creatures gridsize (mkStdGen 1)  (mkStdGen 2), 
        _foods=generateRandomPositions n_foods gridsize (mkStdGen 3)  (mkStdGen 4),
        _gridsize=gridsize
    }

-- | Find nearest Food in the Simulation given a Creature
findNearestFood :: Simulation -> Creature -> Pos
findNearestFood s c = snd $ minimum [ (getDistance f_pos (getCreaturePos c), f_pos) | f_pos <- getFoodPositions s]

-- | Get the distance between two positions
getDistance :: Pos -> Pos -> Int
getDistance p1 p2 = abs (fst p1 - fst p2) + abs (snd p1 - snd p2) 

-- | Move a creature to nearest food, replace in Simulation
moveToClosestFood :: Simulation -> Creature -> Simulation
moveToClosestFood s c = s {_creatures = replace c (moveCreature c $ decideDirection (findNearestFood s c) (getCreaturePos c)) $ getCreatures s}

-- | Decide a direction (Up',Down',Right',Left') based on current and target position
decideDirection :: Pos -> Pos -> Direction 
decideDirection curr_pos target_pos
    | fst curr_pos /= fst target_pos =
      if fst curr_pos < fst target_pos then Up' else Down'
    | snd curr_pos < snd target_pos = Right'
    | otherwise = Left'

-- | Replace the position of a creature based on the given Direction
moveCreature :: Creature -> Direction -> Creature
moveCreature c Up' = c { _pos = changePos (getCreaturePos c) 1 1};
moveCreature c Down' = c { _pos = changePos (getCreaturePos c) 1 (-1)};
moveCreature c Left' = c { _pos = changePos (getCreaturePos c) 0 1};
moveCreature c Right' = c { _pos = changePos (getCreaturePos c) 0 (-1)};

-- | Alter the position on the given axis with a given value
changePos :: Pos -> Int -> Int -> Pos
changePos (x,y) 0 value = (x+value,y)
changePos (x,y) 1 value = (x,y+value)

replace :: Eq a =>
           a   -- ^ Value to look for
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)


-- | do Step forward for each creature
stepForward :: Simulation -> Simulation
stepForward s = foldl moveToClosestFood s $ getCreatures s