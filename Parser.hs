module Parser (
	toyParse
) where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language ( haskell )
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Expr

import Value

toyParse :: String -> Either ParseError Value
toyParse = parse ( do { ret <- parser; eof; return ret } ) ""

infixParser :: Parser Value
infixParser = buildExpressionParser exprTable parser <?> "expression"

parser :: Parser Value
parser = do
	a <- parserAtom
	f <- parser'
	return $ f a

parser' :: Parser ( Value -> Value )
parser' = do	a <- parserAtom
		f <- parser'
		return $ \v -> f $ Apply v a
	<|> return id

parserAtom :: Parser Value
parserAtom =
	lambda <|>
	parens infixParser <|>
	expr <|>
	liftM Integer integer <|>
	liftM String stringLiteral <|>
	liftM Identifier identifier

expr :: Parser Value
expr = buildExpressionParser exprTable factor <?> "expression"

exprTable = [
	[ op "+" AssocLeft, op "-" AssocLeft ]
 ]
	where
	op s assoc = Infix ( reservedOp s >>
		return ( \x y -> Apply ( Apply ( Identifier s ) x ) y ) <?>
			"operator" ) assoc
	
factor :: Parser Value
factor = parens expr <|> liftM Integer integer <?> "simple expression"

lambda :: Parser Value
lambda = do
	reservedOp "\\"
	is <- many1 identifier
	reservedOp "->"
	body <- parser
	return $ Lambda [ ] is body

identifier = lexeme $ many1 letter
-- identifier	= T.identifier haskell
reserved 	= T.reserved haskell
operator	= T.operator haskell
reservedOp	= T.reservedOp haskell
charLiteral	= T.charLiteral haskell
stringLiteral	= T.stringLiteral haskell
natural		= T.natural haskell
integer		= T.integer haskell
float		= T.float haskell
naturalOrFloat	= T.naturalOrFloat haskell
decimal		= T.decimal haskell
hexadecimal	= T.hexadecimal haskell
octal		= T.octal haskell
symbol		= T.symbol haskell
lexeme		= T.lexeme haskell
whiteSpace	= T.whiteSpace haskell
parens		= T.parens haskell
braces		= T.braces haskell
angles		= T.angles haskell
brackets	= T.brackets haskell
squares		= T.squares haskell
semi		= T.semi haskell
comma		= T.comma haskell
colon		= T.colon haskell
dot		= T.dot haskell
semiSep		= T.semiSep haskell
semiSep1	= T.semiSep1 haskell
commaSep	= T.commaSep haskell
commaSep1	= T.commaSep1 haskell
