{-|
Module      : Main
Description : Main Module for Declaretive Programming, University of Applied Sciences Utrecht
Copyright   : (c) Berry Hijwegen, 2019
License     : GPL-3
Maintainer  : berry.hijwegen@student.hu.nl
Stability   : experimental
Portability : POSIX
-}

module Main where

import Helpers (readUntilNumeric)
import Simulation (printSimulation, generatePlatform, stepForward)

-- | Main application function. Asks user for number of creatures, foods, grid size and seed for the Simulation to be generated. Will be prompted until a valid number is given.
main :: IO ()
main = do
    putStrLn "How many creatures do you want to spawn? Note: You will be prompted to type until a valid number is given."
    number_of_creatures <- readUntilNumeric
    putStrLn "How many food items do you want to spawn? Note: You will be prompted to type until a valid number is given."
    number_of_foods <- readUntilNumeric 
    putStrLn "How big does the grid has to be? (Insert one number) Note: You will be prompted to type until a valid number is given."
    grid_size <- readUntilNumeric
    putStrLn "Fill in a number to generate a pseudo-random simulation. Note: You will be prompted to type until a valid number is given."    
    seed <- readUntilNumeric
    mapM_ printSimulation $ take 100 $ iterate stepForward $ generatePlatform (read number_of_creatures) (read number_of_foods) (read grid_size) (read seed)