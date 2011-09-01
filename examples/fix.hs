module Main where

y = \f -> ( \x -> f ( x x ) ) ( \x -> f ( x x ) )

factabs fact x = case x of
	0 -> 1
	_ -> x * fact ( x - 1 )

fac = ( \x y -> case y of { 0 -> 1; _ -> y * ( x x ) ( y - 1 ) } )
	( \x y -> case y of { 0 -> 1; _ -> y * ( x x ) ( y - 1 ) } )
