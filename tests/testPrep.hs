{-# LANGUAGE PackageImports #-}

module Main where

import Lexer
import Types
import System.Environment
import "monads-tf" Control.Monad.State
import Control.Monad.Tools

main :: IO ()
main = do
	[ fn ] <- getArgs
	cnt <- readFile fn
	let proc = doWhile [ ] $ \ts -> do
		t <- prep'
		let c = t /= TokenEOF
		return ( ts ++ [ t ], c )

	print $ proc `evalState` ( 0, [ ], ( 1, 1 ), cnt, [ ] )
