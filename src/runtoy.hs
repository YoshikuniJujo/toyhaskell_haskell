module Main where

import System.Environment ( getArgs )
import MainTools ( mainGen )

main :: IO ()
main = do
	src : args <- getArgs
	mainGen [ "-e", "main", src ] args
