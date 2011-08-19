module BuildExpression (
	buildExprParser,
	OpTable
) where

import Text.ParserCombinators.Parsec hiding ( token )
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Expr

type OpTable t st a = [ ( t, a -> a -> a, Int, Assoc ) ]

mkTableGen :: Eq t => ( t -> String ) -> ( t -> SourcePos ) ->
	[ ( t, a -> a -> a, Int, Assoc ) ] -> OperatorTable t st a
mkTableGen s p = foldr ( uncurry4 $ insertToTableGen s p ) $ replicate 10 [ ]

buildExprParser :: Eq t => ( t -> String ) -> ( t -> SourcePos ) ->
	[ ( t, a -> a -> a, Int, Assoc ) ] -> GenParser t st a ->
	GenParser t st a
buildExprParser s p tbl = buildExpressionParser ( mkTableGen s p tbl )

uncurry4 :: ( a -> b -> c -> d -> e ) -> ( a, b, c, d ) -> e
uncurry4 f ( x, y, z, w ) = f x y z w

insertToTableGen :: Eq t => ( t -> String ) -> ( t -> SourcePos ) ->
	t -> ( a -> a -> a ) -> Int -> Assoc -> OperatorTable t st a ->
	OperatorTable t st a
insertToTableGen s p op f power assoc tbl =
	take pos tbl ++ [ fx : tbl !! pos ] ++ drop ( pos + 1 ) tbl
	where
	pos = 9 - power
	fx = Infix ( mkOpParserGen s p op f ) assoc

mkOpParserGen :: Eq t => ( t -> String ) -> ( t -> SourcePos ) ->
	t -> ( a -> a -> a ) -> GenParser t st ( a -> a -> a )
mkOpParserGen s p op f = P.token s p ( eqM op ) >> return f

eqM :: Eq a => a -> a -> Maybe ()
eqM x y	| x == y	= Just ()
	| otherwise	= Nothing
