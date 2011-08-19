module Main where

import Text.RegexPR
import System.Environment

main :: IO ()
main = do
	[ before, after ] <- getArgs
	interact $ gsubRegexPR before after
