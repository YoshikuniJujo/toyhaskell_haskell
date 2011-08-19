module Interact (
	runLoop
) where

import System.IO ( hFlush, stdout )
import Control.Monad.Tools ( doWhile )

runLoop :: String -> a -> ( a -> String -> IO a ) -> IO ()
runLoop name stat0 act = do
	_ <- doWhile stat0 $ \stat -> do
		input <- prompt $ name ++ "> "
		if input `notElem` [ ":quit", ":q" ]
			then fmap ( flip (,) True ) $ act stat input
			else return ( stat, False )
	putStrLn $ "Leaving " ++ name ++ "."
	
prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine
