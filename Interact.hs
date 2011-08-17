module Interact (
	runLoop
) where

import System.IO ( hFlush, stdout )
import Control.Monad ( when )
import Control.Monad.Tools ( doWhile_ )

runLoop :: String -> ( String -> IO a ) -> IO ()
runLoop name act = do
	doWhile_ $ do
		input <- prompt $ name ++ "> "
		when ( head input /= ':' ) $ act input >> return ()
		return $ input `notElem` [ ":quit", ":q" ]
	putStrLn $ "Leaving " ++ name ++ "."
	
prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine
