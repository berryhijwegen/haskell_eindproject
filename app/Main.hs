module Main where

import SimulationModule

main :: IO ()
main = do
    initialState <- generatePlatform 5 5
    iterate stepForward initialState