module BuildExpression (
	buildExprParser,
	OpTable
) where

import Text.ParserCombinators.Parsec hiding ( token )
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Expr

type OpTable t st a = [ ( t, a -> a -> a, Int, Assoc ) ]

mkTableGen :: Eq b => ( t -> String ) -> ( t -> SourcePos ) -> ( t -> b ) ->
	[ ( t, a -> a -> a, Int, Assoc ) ] -> OperatorTable t st a
mkTableGen s p pre = foldr ( uncurry4 $ insertToTableGen s p pre ) $ replicate 10 [ ]

buildExprParser :: Eq b => ( t -> String ) -> ( t -> SourcePos ) -> ( t -> b ) ->
	[ ( t, a -> a -> a, Int, Assoc ) ] -> GenParser t st a ->
	GenParser t st a
buildExprParser s p pre tbl = buildExpressionParser ( mkTableGen s p pre tbl )

uncurry4 :: ( a -> b -> c -> d -> e ) -> ( a, b, c, d ) -> e
uncurry4 f ( x, y, z, w ) = f x y z w

insertToTableGen :: Eq b => ( t -> String ) -> ( t -> SourcePos ) -> ( t -> b ) ->
	t -> ( a -> a -> a ) -> Int -> Assoc -> OperatorTable t st a ->
	OperatorTable t st a
insertToTableGen s p pre op f power assoc tbl =
	take pos tbl ++ [ fx : tbl !! pos ] ++ drop ( pos + 1 ) tbl
	where
	pos = 9 - power
	fx = Infix ( mkOpParserGen s p pre op f ) assoc

mkOpParserGen :: Eq b => ( t -> String ) -> ( t -> SourcePos ) -> ( t -> b ) ->
	t -> ( a -> a -> a ) -> GenParser t st ( a -> a -> a )
mkOpParserGen s p pre op f = P.token s p ( eqM ( pre op ) . pre ) >> return f

eqM :: Eq a => a -> a -> Maybe ()
eqM x y	| x == y	= Just ()
	| otherwise	= Nothing
