module Main where

import System.Environment ( getArgs )
import System.Console.GetOpt ( OptDescr( .. ), getOpt', ArgOrder( .. ),
	ArgDescr( .. ) )
import MainTools ( mainGen )

main :: IO ()
main = do
	( opts, src : args, rests, _ ) <-
		fmap ( getOpt' RequireOrder options ) getArgs
	let opTblPath = case filter isOpTable opts of
		[ ]		-> [ ]
		[ OpTable otp ]	-> [ "--op-table", otp ]
		_		-> error "multiple --op-table not allowed"
	mainGen ( "-e" : "main" : opTblPath ++ [ src ] ) $ rests ++ args

data Option = OpTable { getOpTable :: String }

isOpTable :: Option -> Bool
isOpTable ( OpTable _ ) = True

options :: [ OptDescr Option ]
options = [
	Option "" [ "op-table" ] ( ReqArg OpTable "operation table path" )
	"set operation table"
 ]
