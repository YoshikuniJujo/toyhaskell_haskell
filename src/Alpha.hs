{-# LANGUAGE TupleSections #-}

module Alpha ( toyAlpha, toyAlphaEnv ) where

import Value (
	Value( Var, Comp, App, Lambda, Case, Letin, Let, Module ),
	Pattern( PatVar, PatCon ), patVars, Env, mapEnv,
	Var( V ) )
import Data.List ( intersect, union )
import Control.Arrow ( second, (***), (&&&) )

--------------------------------------------------------------------------------

toyAlpha :: [ String ] -> Value -> Value
toyAlpha = alpha

toyAlphaEnv :: [ Pattern ] -> Env -> Env
toyAlphaEnv = flip ( foldr alphaEnv )  . ( patVars `concatMap` )

alphaEnv :: String -> Env -> Env
alphaEnv v = mapEnv ( succVar v ) ( succVar v ) ( succVar v )

alpha :: [ String ] -> Value -> Value
alpha pre ( Comp con mems )	= Comp con $ alpha pre `map` mems
alpha pre ( App fun arg )	= App ( alpha pre fun ) $ alpha pre arg
alpha pre ( Lambda ps ex )	= let ( dups, np ) = getDP pre ps in
	Lambda ( mapSuccVars dups ps ) $ alpha np $ succVars dups ex
alpha pre ( Case key sels )	= Case ( alpha pre key ) $ ac `map` sels
	where ac sel@( cond, _ ) = let ( dups, np ) = getDP pre [ cond ] in
		second ( alpha np ) $ succVars dups sel
alpha pre ( Letin defs ex )	= let ( dups, np ) = getDP pre $ map fst defs in
	Letin ( alphaDefs pre defs ) $ alpha np $ succVars dups ex
alpha pre ( Module defs )	= Module $ alphaDefs pre defs
alpha pre ( Let defs )		= Let $ second ( alpha pre ) `map` defs
alpha _ v			= v

alphaDefs :: [ String ] -> [ ( Pattern, Value ) ] -> [ ( Pattern, Value ) ]
alphaDefs pre defs		= let ( dups, np ) = getDP pre $ map fst defs in
	( second ( alpha np ) . succVars dups ) `map` defs

getDP :: [ String ] -> [ Pattern ] -> ( [ String ], [ String ] )
getDP = curry $
	( uncurry intersect &&& uncurry union ) . second ( patVars `concatMap` )

mapSuccVars :: Alpha sv => [ String ] -> [ sv ] -> [ sv ]
mapSuccVars = map . succVars

succVars :: Alpha sv => [ String ] -> sv -> sv
succVars = flip $ foldr succVar

class Alpha sv where
	succVar	:: String -> sv -> sv

instance Alpha Var where
	succVar v v1@( V x n )
		| v == x	= V x $ n + 1
		| otherwise	= v1

instance Alpha Pattern where
	succVar v ( PatVar x n )
		| v == x		= PatVar x $ n + 1
	succVar v ( PatCon c mems )	= PatCon c $ succVar v `map` mems
	succVar _ p			= p

instance Alpha Value where
	succVar v ( Lambda ps ex )	=
		Lambda ( succVar v `map` ps ) $ succVar v ex
	succVar v ( Var x n )
		| v == x		= Var x $ n + 1
	succVar v ( App f a )		= App ( succVar v f ) ( succVar v a )
	succVar v ( Comp con mems )	= Comp con $ succVar v `map` mems
	succVar v ( Case key sels )	=
		Case ( succVar v key ) ( succVar v `map` sels )
	succVar v ( Letin defs ex )	=
		Letin ( succVar v `map` defs ) $ succVar v ex
	succVar _ v			= v

instance ( Alpha a, Alpha b ) => Alpha ( a, b ) where
	succVar vars	= succVar vars *** succVar vars
