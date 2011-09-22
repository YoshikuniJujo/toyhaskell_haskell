module Lexer ( Token( .. ), lexer, lexeme ) where

import Control.Arrow ( first )

data Token =
	TokInteger Integer	|
	TokChar Char		|
	TokString String	|
	Special Char		|
	ReservedOp String	|
	ReservedId String	|
	VarSym String		|
	ConSym String		|
	VarId String		|
	ConId String		|
	NewLine			|
	TokEOF			|
	AddBrace Int		|
	Indent Int
	deriving ( Show, Eq )

small, large, symbol, digit, special :: String
small	= "abcdefghijklmnopqrstuvwxyz_"
large	= "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
symbol	= "!#$%&*+./<=>?@\\^|-~:"
digit	= "0123456789"
special = "(),;[]`{}"

reservedId, reservedOp :: [ String ]
reservedId = [
	"case", "class", "data", "default", "deriving", "do", "else", "foreign",
	"if", "import", "in", "infix", "infixl", "infixr", "instance", "let",
	"module", "newtype", "of", "then", "type", "where", "_"
 ]
reservedOp = [ "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>" ]

type Lexer = String -> ( Token, ( String, String ) )

lexer :: Lexer
lexer ""			= ( TokEOF, ( "", "" ) )
lexer ( '\n' : cs )		= ( NewLine, ( "\n", cs ) )
lexer ( '\'' : cs )		= lexerChar cs
lexer ( '"' : cs )		= lexerString cs
lexer ca@( c : cs )
	| c `elem` special	= ( Special c, ( [ c ], cs ) )
	| c `elem` small	= spanToken ( small ++ large ++ digit ) mkTkV
	| c `elem` large	= spanToken ( small ++ large ++ digit ) ConId
	| c `elem` ":"		= spanToken symbol mkTkC
	| c `elem` symbol	= spanToken symbol mkTkO
	| c `elem` digit	= spanToken digit ( TokInteger . read )
        | otherwise		= error $ "lexer failed: " ++ ca
	where
	spanToken chType f = let ( ret, rest ) = span ( `elem` chType ) ca in
		( f ret, ( ret, rest ) )
	mkTkV v	= ( if v `elem` reservedId then ReservedId else VarId ) v
	mkTkO o	= ( if o `elem` reservedOp then ReservedOp else VarSym ) o
	mkTkC o = ( if o `elem` reservedOp then ReservedOp else ConSym ) o

lexerChar :: Lexer
lexerChar ca = let ( ret, '\'' : rest ) = span ( /= '\'' ) ca in
	( TokChar $ readChar ret, ( '\'' : ret ++ "'", rest ) )
	where
	readChar "\\n"	= '\n'
	readChar [ c ]	= c
	readChar _	= error "bad charactor literal"

lexerString :: Lexer
lexerString ca = let ( ret, '"' : rest ) = span ( /= '"' ) ca in
	( TokString ret, ( '"' : ret ++ "\"", rest ) )

lexeme :: Lexer -> Lexer
lexeme lx src = let
	( t, ( lexed, rest ) )	= lx src
	( ws, rest' )		= gw rest in
	( t, ( lexed ++ ws, rest' ) )
	where
	gw ca@( '-' : '-' : _ )	= first ( c ++ ) $ gw r
		where ( c, r ) = span ( /= '\n' ) ca
	gw ( ' ' : cs )		= first ( ' ' : ) $ gw cs
	gw ( '\t' : cs )	= first ( '\t' : ) $ gw cs
	gw ca			= ( "", ca )
