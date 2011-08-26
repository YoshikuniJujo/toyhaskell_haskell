module Main where

import System.Environment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Data.Char

main :: IO ()
main = do
	[ fn ] <- getArgs
	cnt <- readFile fn
	print $ testParse $ prep $ lexer cnt

data Token =
	Let | OB | CB | In | Equal | Var String | Int Int | LOB
	deriving ( Show, Eq )

getVar :: Token -> Maybe String
getVar ( Var v )	= Just v
getVar _		= Nothing

getInt :: Token -> Maybe Int
getInt ( Int i )	= Just i
getInt _		= Nothing

data Parsed = Parsed ( String, Parsed ) String | PInt Int
	deriving Show

testParse :: [ Token ] -> Either ParseError Parsed
testParse = runParser parser 0 ""

tok :: ( Token -> Maybe a ) -> GenParser Token Int a
tok = token show ( const $ initialPos "" )

tk :: Token -> GenParser Token Int ()
tk t = token show ( const $ initialPos "" ) $ boolM . ( == t )

boolM b = if b then Just () else Nothing

parser :: GenParser Token Int Parsed
parser = do	tk Let
		b <- ( tk OB >> return False ) <|> ( tk LOB >> return True )
		v <- tok getVar
		tk Equal
		i <- parser
		if b then tk CB <|> return () else tk CB
		tk In
		v' <- tok getVar
		return $ Parsed ( v, i ) v'
	<|> fmap PInt ( tok getInt )

prep :: [ Token ] -> [ Token ]
prep [ ]		= [ ]
prep ( Let : OB : ts )	= Let : OB : prep ts
prep ( Let : ts )	= Let : LOB : prep ts
prep ( t : ts )		= t : prep ts

lexer :: String -> [ Token ]
lexer ""			= [ ]
lexer "\n"			= [ ]
lexer ( ' ' : cs )		= lexer cs
lexer ( '{' : cs )		= OB : lexer cs
lexer ( '}' : cs )		= CB : lexer cs
lexer ( '=' : cs )		= Equal : lexer cs
lexer ( 'l' : 'e' : 't' : cs )	= Let : lexer cs
lexer ( 'i' : 'n' : cs )	= In : lexer cs
lexer cs@( c : _ )
	| isLower c		= let ( ret, rest ) = span isLower cs in
					Var ret : lexer rest
	| isDigit c		= let ( ret, rest ) = span isDigit cs in
					Int ( read ret ) : lexer rest
