{-|
Module      : Position
Description : Position Module for Declaretive Programming, University of Applied Sciences Utrecht
Copyright   : (c) Berry Hijwegen, 2019
License     : GPL-3
Maintainer  : berry.hijwegen@student.hu.nl
Stability   : experimental
Portability : POSIX
-}

module Position (
    getDistance, changePos, generateRandomPositions, decideDirection, Pos, Direction (..)
    ) where

import System.Random (StdGen)
import Helpers (randomList)

-- | datatype to make Directions more readable and understandable according to the grid.
data Direction = Up' | Down' | Left' | Right'

-- | Pathfinding to decide a direction (Up',Down',Right',Left') based on current and target position
decideDirection :: Pos							-- ^ Input: Current position
                -> Pos 							-- ^ Input: Target position
                -> Direction 					-- ^ Output: Direction based on decision making
decideDirection curr_pos target_pos
    | snd curr_pos /= snd target_pos =
      if snd curr_pos < snd target_pos then Up' else Down'
    | fst curr_pos < fst target_pos = Right'
    | otherwise = Left'

-- | Alternative notation of (Int,Int) to make code more readable
type Pos = (Int, Int)

-- | Get the distance between two Positions
getDistance :: Pos 								-- ^ Input: Position 1
			-> Pos 								-- ^ Input: Position 2 
			-> Int 								-- ^ Output: Absolute distance between two Positions
getDistance p1 p2 = abs (fst p1 - fst p2) + abs (snd p1 - snd p2) 

-- | Alter the position on the given axis with a given value
changePos :: Pos 								-- ^ Input: Position to change
		  -> Int 								-- ^ Input: Axis/index to change
		  -> Int 								-- ^ Input: Number to add (positive or negative) to regarding index
		  -> Pos								-- ^ Output: Given position changed based on axis and number
changePos (x,y) 0 value = (x+value,y)
changePos (x,y) 1 value = (x,y+value)

-- | Create list of pseudo-random Positions using two seeds
generateRandomPositions :: Int 					-- ^ Input: Number of pseudo-random Positions to be generated
						-> Int 					-- ^ Input: Highest allowed value in to be generated
						-> StdGen 				-- ^ Input: Seed 1 (For x position)
						-> StdGen 				-- ^ Input: Seed 2 (For y position)
						-> [Pos]				-- ^ Output: List of pseudo-random generated Positions
generateRandomPositions n max seed1 seed2 = zip (randomList n max seed1) (randomList n max seed2)