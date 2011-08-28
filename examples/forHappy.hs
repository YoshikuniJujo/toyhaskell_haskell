module Main where

hello	= "Hello, world!\n"
bye	= "Good-bye, world\n"

letTest	= let x = 3; y = x + 5; z = x + y in 2 * z

letTestt = let
		x = 3
		z = x
		y = 4
	in ( x + y ) + z
