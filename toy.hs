module Main where

import System.IO
import System.Environment
import Control.Monad.Tools
import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Either

fromRight ( Right x ) = x

main :: IO ()
main = do
	[ fn ] <- getArgs
	parsed <- liftM ( map ( fromRight . parse parseDef "toy" ) . lines ) $
		readFile fn
	let pairs = map parsedToPair parsed
	doWhile_ $ do
		input <- prompt "toy> "
		maybe ( putStrLn $ "Not in scope: `" ++ input ++ "'" ) ( print ) $
			lookup input pairs
		return $ input /= ":quit"

parsedToPair :: Parsed -> ( String, String )
parsedToPair ( PDef ( PSymbol sym ) ( PString str ) ) = ( sym, str )

prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine

data Code = Apply Code Code | Value deriving Show

data Value = Int Int | Primitive deriving Show

data Primitive = PrimitiveIntIntInt ( Int -> Int -> Int )

instance Show Primitive where
	show _ = "<primitive>"

data Parsed = PDef PPat PBody deriving Show

data PPat = PSymbol String deriving Show
data PBody = PString String deriving Show

parseDef :: Parser Parsed
parseDef = do
	sym <- many lower
	spaces
	char '='
	spaces
	char '"'
	body <- many $ noneOf "\""
	char '"'
	return $ PDef ( PSymbol sym ) ( PString body )
