module BuildExpression (
	buildExprParser,
	Op,
	Assoc( .. )
) where

import Text.ParserCombinators.Parsec ( GenParser )
import Text.ParserCombinators.Parsec.Expr (
	buildExpressionParser, OperatorTable, Operator( .. ), Assoc( .. ) )

type Op t a	= ( t, OpFun a, Int, Assoc )
type OpFun a	= a -> a -> a
type Token t st	= ( t -> Maybe () ) -> GenParser t st ()
type Compare t	= t -> t -> Bool
type P		= GenParser
type T t st a	= OperatorTable t st a

buildExprParser :: Token t st -> Compare t -> [ Op t a ] -> P t st a -> P t st a
buildExprParser tk eq tbl = buildExpressionParser ( mkTable tk eq tbl )

mkTable :: Token t st -> Compare t -> [ Op t a ] -> T t st a
mkTable tk eq = foldr ( insertToTable tk eq ) $ replicate 10 [ ]

insertToTable :: Token t st -> Compare t -> Op t a -> T t st a -> T t st a
insertToTable tk eq ( op, f, power, assoc ) =
	insertToI ( 9 - power ) ( Infix ( mkOpParser tk eq op f ) assoc )

mkOpParser :: Token t st -> Compare t -> t -> OpFun a -> P t st ( OpFun a )
mkOpParser tk eq op f = tk ( boolM . eq op ) >> return f

boolM :: Bool -> Maybe ()
boolM True	= Just ()
boolM False	= Nothing

insertToI :: Int -> a -> [ [ a ] ] -> [ [ a ] ]
insertToI i x xs = take i xs ++ [ x : xs !! i ] ++ drop ( i + 1 ) xs
