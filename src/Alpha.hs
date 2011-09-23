{-# LANGUAGE TupleSections #-}

module Alpha ( alpha, alphaEnv ) where

import Value (
	Value( Var, Comp, App, Lambda, Case, Letin, Let, Module ),
	Pattern( PatVar, PatCon ), patVars, Env, mapEnv, Var( V ) )
import Data.List ( intersect, union )
import Control.Arrow ( second, (***), (&&&) )

--------------------------------------------------------------------------------

alphaEnv :: [ Pattern ] -> Env -> Env
alphaEnv = flip ( foldr ae )  . ( patVars `concatMap` )
	where ae v = mapEnv ( succV v ) ( succV v ) ( succV v )

alpha :: [ String ] -> Value -> Value
alpha pre ( Comp nm ms )	= Comp nm $ alpha pre `map` ms
alpha pre ( App f a )		= App ( alpha pre f ) $ alpha pre a
alpha pre ( Lambda ps e )	= let ( dups, np ) = getDP pre ps in
	Lambda ( map ( succVs dups ) ps ) $ alpha np $ succVs dups e
alpha pre ( Case k alts )	= Case ( alpha pre k ) $ ac `map` alts
	where ac alt@( test, _ ) = let ( dups, np ) = getDP pre [ test ] in
		second ( alpha np ) $ succVs dups alt
alpha pre ( Letin ds e )	= let ( dups, np ) = getDP pre $ map fst ds in
	Letin ( alphaDefs pre ds ) $ alpha np $ succVs dups e
alpha pre ( Module ds )		= Module $ alphaDefs pre ds
alpha pre ( Let ds )		= Let $ second ( alpha pre ) `map` ds
alpha _ v			= v

alphaDefs :: [ String ] -> [ ( Pattern, Value ) ] -> [ ( Pattern, Value ) ]
alphaDefs pre ds		= let ( dups, np ) = getDP pre $ map fst ds in
	( second ( alpha np ) . succVs dups ) `map` ds

getDP :: [ String ] -> [ Pattern ] -> ( [ String ], [ String ] )
getDP = curry $
	( uncurry intersect &&& uncurry union ) . second ( patVars `concatMap` )

succVs :: Alpha sv => [ String ] -> sv -> sv
succVs = flip $ foldr succV

class Alpha sv where
	succV :: String -> sv -> sv

instance Alpha Var where
	succV v v1@( V x n )
		| v == x	= V x $ n + 1
		| otherwise	= v1

instance Alpha Pattern where
	succV v ( PatVar x n )
		| v == x	= PatVar x $ n + 1
	succV v ( PatCon c ms )	= PatCon c $ succV v `map` ms
	succV _ p		= p

instance Alpha Value where
	succV v ( Var x n )
		| v == x	= Var x $ n + 1
	succV v ( Comp nm ms )	= Comp nm $ succV v `map` ms
	succV v ( App f a )	= App ( succV v f ) ( succV v a )
	succV v ( Lambda ps e )	= Lambda ( succV v `map` ps ) $ succV v e
	succV v ( Case k alts )	= Case ( succV v k ) ( succV v `map` alts )
	succV v ( Letin ds e )	= Letin ( succV v `map` ds ) $ succV v e
	succV _ val		= val

instance ( Alpha a, Alpha b ) => Alpha ( a, b ) where
	succV vars = succV vars *** succV vars
