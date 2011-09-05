module Main where

putStr str = case str of
	c : cs	-> putChar c >> putStr cs
	[ ]	-> return ()

putStrLn str = putStr str >> putChar '\n'
