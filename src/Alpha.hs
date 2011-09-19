{-# LANGUAGE TupleSections #-}

module Alpha ( alpha, alphaEnv ) where

import Value (
	Value( Var, Comp, App, Lambda, Case, Letin, Let, Module ),
	Pattern( PatVar, PatCon ), patVars, Env, mapEnv,
	Var( V ) )
import Data.List ( intersect, union )
import Control.Arrow ( second, (***), (&&&) )

--------------------------------------------------------------------------------

alphaEnv :: [ Pattern ] -> Env -> Env
alphaEnv = flip ( foldr ae )  . ( patVars `concatMap` )
	where ae v = mapEnv ( succV v ) ( succV v ) ( succV v )

alpha :: [ String ] -> Value -> Value
alpha pre ( Comp con mems )	= Comp con $ alpha pre `map` mems
alpha pre ( App fun arg )	= App ( alpha pre fun ) $ alpha pre arg
alpha pre ( Lambda ps ex )	= let ( dups, np ) = getDP pre ps in
	Lambda ( map ( succVs dups ) ps ) $ alpha np $ succVs dups ex
alpha pre ( Case key sels )	= Case ( alpha pre key ) $ ac `map` sels
	where ac sel@( cond, _ ) = let ( dups, np ) = getDP pre [ cond ] in
		second ( alpha np ) $ succVs dups sel
alpha pre ( Letin defs ex )	= let ( dups, np ) = getDP pre $ map fst defs in
	Letin ( alphaDefs pre defs ) $ alpha np $ succVs dups ex
alpha pre ( Module defs )	= Module $ alphaDefs pre defs
alpha pre ( Let defs )		= Let $ second ( alpha pre ) `map` defs
alpha _ v			= v

alphaDefs :: [ String ] -> [ ( Pattern, Value ) ] -> [ ( Pattern, Value ) ]
alphaDefs pre defs		= let ( dups, np ) = getDP pre $ map fst defs in
	( second ( alpha np ) . succVs dups ) `map` defs

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
		| v == x		= PatVar x $ n + 1
	succV v ( PatCon c mems )	= PatCon c $ succV v `map` mems
	succV _ p			= p

instance Alpha Value where
	succV v ( Lambda ps e )	= Lambda ( succV v `map` ps ) $ succV v e
	succV v ( Var x n )
		| v == x	= Var x $ n + 1
	succV v ( App f a )	= App ( succV v f ) ( succV v a )
	succV v ( Comp c ms )	= Comp c $ succV v `map` ms
	succV v ( Case k s )	= Case ( succV v k ) ( succV v `map` s )
	succV v ( Letin ds e )	= Letin ( succV v `map` ds ) $ succV v e
	succV _ v		= v

instance ( Alpha a, Alpha b ) => Alpha ( a, b ) where
	succV vars	= succV vars *** succV vars
