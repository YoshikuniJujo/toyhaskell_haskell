module Main where

import MainTools ( mainGen )
import System.Environment ( getArgs )
import System.Console.GetOpt

main :: IO ()
main = do
	( opts, src : args, rests, errs ) <-
		fmap ( getOpt' RequireOrder options ) getArgs
	let opTblPath = case filter isOpTable opts of
		[ ] -> [ ]
		[ OpTable otp ] -> [ "--op-table", otp ]
		_ -> error "bad"
	mainGen ( "-e" : "main" : opTblPath ++ [ src ] ) $ rests ++ args

data Option = OpTable { getOpTable :: String }
isOpTable ( OpTable _ ) = True

options :: [ OptDescr Option ]
options = [
	Option "" [ "op-table" ] ( ReqArg OpTable "operation table path" )
	"set operation table"
 ]
