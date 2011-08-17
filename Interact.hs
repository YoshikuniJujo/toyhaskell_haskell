module Interact (
	runLoop
) where

import System.IO ( hFlush, stdout )
import Control.Monad ( when )
import Control.Monad.Tools ( doWhile )

runLoop :: String -> a -> ( a -> String -> IO a ) -> IO ()
runLoop name stat0 act = do
	doWhile stat0 $ \stat -> do
		input <- prompt $ name ++ "> "
		stat' <- act stat input
		return ( stat', input `notElem` [ ":quit", ":q" ] )
	putStrLn $ "Leaving " ++ name ++ "."
	
prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine
