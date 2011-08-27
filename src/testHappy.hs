{-# LANGUAGE PackageImports #-}

module Main where

import Interact
import NewParser
import Eval
import Primitives
import Types
import "monads-tf" Control.Monad.State
import Data.Char
import Data.List
import System.Environment

main :: IO ()
main = do
	[ fn ] <- getArgs
	env0 <- loadFile initEnv fn
	runLoop "testHappy" env0 $ \env input -> case input of
		':' : cmd	-> runCmd cmd env
		_		->
			case eval env $ evalState toyParse ( 0, [ ], ( 1, 1 ), input, [ ] ) of
				Let ps	-> return $ setPats ps env
				ret	-> showValue ret >> return env

runCmd :: String -> Env -> IO Env
runCmd cmd env
	| "load" `isPrefixOf` cmd	= do
		let fn = dropWhile isSpace $ drop 4 cmd
		loadFile env fn
	| otherwise			= do
		putStrLn $ "unknown command : '" ++ cmd ++ "'"
		return env

loadFile :: Env -> FilePath -> IO Env
loadFile env fn = do
	cnt <- readFile fn
	case eval env $ evalState toyParseModule ( 0, [ ], (1, 1 ), cnt, [ ] ) of
		Let ps	-> return $ setPats ps env
		bad	-> error $ show bad
