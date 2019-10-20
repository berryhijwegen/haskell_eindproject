module Main where

import Lib2

main :: IO ()
main = do
    initialState <- generatePlatform
    iterate stepForward initialState