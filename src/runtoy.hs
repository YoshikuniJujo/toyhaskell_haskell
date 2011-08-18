module Main where

import System.Environment ( getArgs )
import MainTools ( mainGen )

main :: IO ()
main = do
	[ fn ] <- getArgs
	mainGen [ "-e", "main", fn ]
