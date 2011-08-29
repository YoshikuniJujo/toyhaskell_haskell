module Main where

import MainTools ( mainGen )
import System.Environment ( getArgs )

main :: IO ()
main = getArgs >>= \( src : args ) -> mainGen ( "-e" : "main" : [ src ] ) args
