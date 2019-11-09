{-|
Module      : Creature
Description : Creature Module for Declaretive Programming, University of Applied Sciences Utrecht
Copyright   : (c) Berry Hijwegen, 2019
License     : GPL-3
Maintainer  : berry.hijwegen@student.hu.nl
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE ParallelListComp #-}

module Creature (
    getCreatureInfo, getHp, alterHp, moveCreature, Creature, getCreaturePos, generateCreatures
    ) where

import Position (changePos, Direction (..), Pos, generateRandomPositions)
import System.Random (StdGen)

-- | A Creature will walk through the grid searching for food, trying not to die.
data Creature = Creature 
  { _hp             :: Int                    -- ^ Number of health point of the Creature, default 100.
  , _pos            :: Pos                    -- ^ Position of the Creature in the grid
  } deriving (Show, Eq, Ord)

-- | generate creatures with a pseudo-random position using two seeds
generateCreatures :: Int                      -- ^ Input: Number of Creatures to create
                  -> Int                      -- ^ Input: Highest allowed position in the grid
                  -> StdGen                   -- ^ Input: Seed 1 (For x position)
                  -> StdGen                   -- ^ Input: Seed 2 (For y position)
                  -> [Creature]               -- ^ Output: Newly generated Creatures
generateCreatures n max seed1 seed2 = [Creature {_hp=100,_pos=pos} | i <- [1..n] | pos <- generateRandomPositions n max seed1 seed2] 

getCreatureInfo :: Creature                   -- ^ Input: Creature to get info from
                -> String                     -- ^ Output: All info about the given Creature in one String
getCreatureInfo (Creature _hp _pos) = "Creature has " ++ show _hp ++ "hp and it's position is (" ++ show (fst _pos) ++ "," ++ show (snd _pos) ++")."

-- | Returns positions of given Creature
getCreaturePos :: Creature                    -- ^ Input: Creature to get position from
               -> Pos                         -- ^ Output: Position in (x,y) format
getCreaturePos (Creature _ _pos) = _pos

-- | Retrieve hp level of given Creature
getHp :: Creature                             -- ^ Input: Creature to get hp level from
      -> Int                                  -- ^ Output: Hp level of given Creature
getHp (Creature _hp _) = _hp

-- | Increase or decrease hp of given Creature
alterHp :: Int                                -- ^ Input: Number of hp to add to current hp level, can be positive or negative
        -> Creature                           -- ^ Input: Creature to alter hp for
        -> Creature                           -- ^ Output: Given Creature with altered hp level
alterHp v c = c {_hp = (getHp c) + v}

-- | Replace the position of a creature based on the given Direction
moveCreature :: Creature                      -- ^ Input: Creature to change position for
             -> Direction                     -- ^ Input: Direction to move to. Can be Up', Down', Left', Right'
             -> Creature                      -- ^ Output: Given Creature with +1 in given Direction
moveCreature c Up' = c { _pos = changePos (getCreaturePos c) 1 1};
moveCreature c Down' = c { _pos = changePos (getCreaturePos c) 1 (-1)};
moveCreature c Left' = c { _pos = changePos (getCreaturePos c) 0 (-1)};
moveCreature c Right' = c { _pos = changePos (getCreaturePos c) 0 1};