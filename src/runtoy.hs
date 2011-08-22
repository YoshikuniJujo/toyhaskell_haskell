module Main where

import System.Environment ( getArgs )
import MainTools ( mainGen )

main :: IO ()
main = do
	args <- getArgs
	mainGen ( "-e" : "main" : args ) [ ]
