module BuildExpression (
	buildExprParser,
	Op,
	Assoc( .. )
) where

import Text.ParserCombinators.Parsec hiding ( token )
import Text.ParserCombinators.Parsec.Expr (
	buildExpressionParser, OperatorTable, Operator( .. ), Assoc( .. ) )

type Op t a	= ( t, a -> a -> a, Int, Assoc )
type Token t st	= ( t -> Maybe () ) -> GenParser t st ()
type Compare t	= t -> t -> Bool

buildExprParser :: Token t st -> Compare t -> [ Op t a ] -> GenParser t st a ->
	GenParser t st a
buildExprParser tk eq tbl = buildExpressionParser ( mkTable tk eq tbl )

mkTable :: Token t st -> Compare t -> [ Op t a ] -> OperatorTable t st a
mkTable tk eq = foldr ( uncurry4 $ insertToTable tk eq ) $ replicate 10 [ ]

uncurry4 :: ( a -> b -> c -> d -> e ) -> ( a, b, c, d ) -> e
uncurry4 f ( x, y, z, w ) = f x y z w

insertToTable :: Token t st -> Compare t -> t -> ( a -> a -> a ) -> Int ->
	Assoc -> OperatorTable t st a -> OperatorTable t st a
insertToTable tk eq op f power assoc tbl =
	take pos tbl ++ [ fx : tbl !! pos ] ++ drop ( pos + 1 ) tbl
	where
	pos = 9 - power
	fx = Infix ( mkOpParser tk eq op f ) assoc

mkOpParser :: Token t st -> Compare t -> t -> ( a -> a -> a ) ->
	GenParser t st ( a -> a -> a )
mkOpParser tk eq op f = tk ( boolM . eq op ) >> return f

boolM :: Bool -> Maybe ()
boolM True	= Just ()
boolM False	= Nothing
