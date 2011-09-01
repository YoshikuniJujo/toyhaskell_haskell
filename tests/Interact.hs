{-# LANGUAGE TupleSections #-}

module Interact ( runLoop ) where

import Control.Monad.Tools
import System.IO

runLoop :: String -> a -> ( a -> String -> IO a ) -> IO ()
runLoop name stat0 act = do
	_ <- doWhile stat0 $ \stat -> do
		input <- prompt $ name ++ "> "
		if input `elem` [ ":quit", ":q" ]
			then return ( stat, False )
			else fmap ( , True ) $ act stat input
	putStrLn $ "Leaving " ++ name ++ "."
	where
	prompt p = putStr p >> hFlush stdout >> getLine
