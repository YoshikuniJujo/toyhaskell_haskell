module Main where

import MainTools ( mainGen )
import System.Environment ( getArgs )
import Control.Arrow ( first )

main :: IO ()
main = do
	( args, pargs ) <- fmap separateArgs getArgs
	mainGen args pargs

separateArgs :: [ String ] -> ( [ String ], [ String ] )
separateArgs [ ]		= ( [ ], [ ] )
separateArgs ( "--" : rest )	= ( [ ], rest )
separateArgs ( arg : args )	= first ( arg : ) $ separateArgs args
