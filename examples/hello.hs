module Main where

putStr str = case str of
	[ ]	-> return ()
	c : cs	-> putChar c >> putStr cs

putStrLn str = putStr str >> putChar '\n'

main = putStrLn "Hello, world!"
