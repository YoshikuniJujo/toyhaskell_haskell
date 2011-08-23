module Main where

import MainTools ( mainGen )
import System.Environment ( getArgs )
import System.Console.GetOpt ( getOpt', ArgOrder( .. ), OptDescr( .. ),
	ArgDescr( .. ) )

main :: IO ()
main = do
	( opts, src : args, rests, _ ) <-
		fmap ( getOpt' RequireOrder options ) getArgs
	let opTblPath =
		maybe [ ] ( ( "--op-table" : ) . ( : [] ) ) $ evalOptions opts
	mainGen ( "-e" : "main" : opTblPath ++ [ src ] ) $ rests ++ args

data Option = OpTable { getOpTable :: String }

evalOptions :: [ Option ] -> Maybe FilePath
evalOptions [ ]				= Nothing
evalOptions ( OpTable path : _ )	= Just path

options :: [ OptDescr Option ]
options = [
	Option "" [ "op-table" ] ( ReqArg OpTable "operation table path" )
	"set operation table" ]
