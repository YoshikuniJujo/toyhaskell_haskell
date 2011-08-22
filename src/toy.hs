module Main where

import System.Environment ( getArgs )
import MainTools ( mainGen )

main :: IO ()
main = do
	( args, pargs ) <- fmap ( span ( /= "--" ) ) getArgs
	mainGen args $ if null pargs then [ ] else tail pargs
