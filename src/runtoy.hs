module Main where

import qualified MainTools as T ( main )
import System.Environment ( getArgs )

main :: IO ()
main = getArgs >>= \( src : args ) -> T.main ( "-e" : "main" : [ src ] ) args
	>>= putStr
