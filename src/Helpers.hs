{-|
Module      : Helpers
Description : Helpers Module for Declaretive Programming, University of Applied Sciences Utrecht
Copyright   : (c) Berry Hijwegen, 2019
License     : GPL-3
Maintainer  : berry.hijwegen@student.hu.nl
Stability   : experimental
Portability : POSIX
-}

module Helpers (
    readUntilNumeric, replace, exitWithMessage, randomList, getRandomSeed
    ) where

import Data.Char (isDigit)
import System.Random (StdGen, mkStdGen, randomR)
import System.Exit (exitSuccess, exitWith)
import System.IO (stderr, hPutStrLn)
import Data.List (unfoldr)


-- | Basic replace function
replace :: Eq a 
        => a                                  -- ^ Input: Value to look for
        -> a                                  -- ^ Input: Value to replace it with
        -> [a]                                -- ^ Input: Input list
        -> [a]                                -- ^ Output: Output list
replace x y = map (\z -> if z == x then y else z)

-- | Exit program with message
exitWithMessage :: String                     -- ^ Input: Message to show to user
                -> IO a                       -- ^ Output: Message printed to console + Exited program     
exitWithMessage msg = hPutStrLn stderr msg >> exitSuccess

-- | Create pseudo-random list of n numbers using a seed
randomList :: Int                             -- ^ Input: Number of Integers to be generated
           -> Int                             -- ^ Input: Highest allowed value in list
           -> StdGen                          -- ^ Input: Seed
           -> [Int]                           -- ^ Output: List of pseudo-random generated Integers
randomList n max = take n . unfoldr (Just . randomR (1,max))

-- | Function to generate multiple random seed from one number
getRandomSeed :: Int                          -- ^ Input: Number to use as seed for random seeds
              -> Int                          -- ^ Input: index of element to take from list
              -> StdGen                       -- ^ Output: seed
getRandomSeed num i = mkStdGen $ randomList i 1000 (mkStdGen num) !! (i-1)

-- | Execute `f` until condition `act` is true
collectUntil :: (Monad m) 
                => m a                        -- ^ Input: Element to check inside monad
                -> (a -> Bool)                -- ^ Input: Function to check if condition is true 
                -> m a                        -- ^ Output: Element validated, still inside monad
collectUntil act f = do
  x <- act
  if f x
    then return x
    else collectUntil act f

-- | Check if given string consists of all digits
isNumeric :: String                          -- ^ String to check if it's numeric
          -> Bool                            -- ^ Returns True if all digits, else False 
isNumeric = all isDigit

-- | Get text from user until input is all numeric
readUntilNumeric :: IO String               -- ^ String to return if all characters are numeric
readUntilNumeric = collectUntil getLine isNumeric