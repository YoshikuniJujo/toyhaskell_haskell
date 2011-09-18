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
alpha pre ( Lambda ps expr )	= let ( dups, np ) = getDP pre ps in
	Lambda ( mapSuccVars dups ps ) $ alpha np $ succVars dups expr
alpha pre ( Case key sels )	= Case ( alpha pre key ) $ ac `map` sels
	where ac sel@( cond, _ ) = let ( dups, np ) = getDP pre [ cond ] in
		second ( alpha np ) $ succVars dups sel
alpha pre ( Letin defs expr )	= let ( dups, np ) = getDP pre $ map fst defs in
	Letin ( alphaDefs pre defs ) $ alpha np $ succVars dups expr
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
	succVar x0 v@( V x1 n )
		| x0 == x1	= V x1 $ n + 1
		| otherwise	= v

instance Alpha Pattern where
	succVar v0 ( PatVar v1 n )
		| v0 == v1		= PatVar v1 $ n + 1
	succVar v0 ( PatCon c pats )	= PatCon c $ succVar v0 `map` pats
	succVar _ p			= p

instance Alpha Value where
	succVar v0 ( Lambda vs expr )	=
		Lambda ( succVar v0 `map` vs ) $ succVar v0 expr
	succVar v0 ( Var v1 n )
		| v0 == v1			= Var v1 $ n + 1
	succVar v0 ( App v1 v2 )		=
		App ( succVar v0 v1 ) ( succVar v0 v2 )
	succVar v0 ( Comp con vs )	=
		Comp con $ succVar v0 `map` vs
	succVar v0 ( Case v1 ps )		=
		Case ( succVar v0 v1 ) ( succVar v0 `map` ps )
	succVar v0 ( Letin ps expr )	=
		Letin ( succVar v0 `map` ps ) $ succVar v0 expr
	succVar _ v			= v

instance ( Alpha a, Alpha b ) => Alpha ( a, b ) where
	succVar vars	= succVar vars *** succVar vars
