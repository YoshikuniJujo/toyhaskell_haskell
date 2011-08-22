module Main where

import System.Environment ( getArgs )
import MainTools ( mainGen )

main :: IO ()
main = getArgs >>= flip mainGen [ ]
