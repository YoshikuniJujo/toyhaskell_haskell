module Main where

import NewParser

main :: IO ()
main = do
	print $ toyParse $ toyLex' "" "33"
	putStrLn "yet"
