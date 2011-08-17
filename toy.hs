module Main where

import Control.Monad.Tools
import Control.Monad
import System.IO
import Text.ParserCombinators.Parsec
import Data.Maybe

main :: IO ()
main = do
	doWhile_ $ do
		input <- prompt "toy> "
		when ( head input /= ':' ) $
			putStrLn $ either show show $ parse parser "toy" input
		return $ input `notElem` [ ":quit", ":q" ]
	putStrLn "Leaving toyhaskell."
	
prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine

parser :: Parser Int
parser = do
	x <- liftM read $ many1 digit
	f <- parser'
	eof
	return $ f x

parser' :: Parser ( Int -> Int )
parser' = do	spaces
		op <- oneOf "+-*"
		spaces
		y <- liftM read $ many1 digit
		f <- parser'
		return $ \x -> fromJust ( lookup op opList ) ( f x ) y
	<|> return id

opList :: [ ( Char, Int -> Int -> Int ) ]
opList = [
	( '+', (+) ),
	( '-', (-) ),
	( '*', (*) )
 ]
