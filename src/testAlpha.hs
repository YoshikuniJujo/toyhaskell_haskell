module Main where

import MainTools
import System.Environment
import Data.Maybe

main :: IO ()
main = do
	args <- getArgs
	testAlpha $ listToMaybe args
