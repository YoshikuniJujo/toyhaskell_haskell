module Main where

import System.Environment
import MainTools

main :: IO ()
main = do
	[ fn ] <- getArgs
	mainGen [ "-e", "main", fn ]
