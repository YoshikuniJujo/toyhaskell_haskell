module Alpha ( toyAlpha, toyAlphaEnv ) where

import Value (
	Value( Var, Comp, App, Lambda, Case, Letin, Let, Module ),
	Pattern( PatVar, PatCon ), Env, mapEnv,
	Var( V, varName, varVol ), mkVar )
import qualified Value ( patVars )
import Data.List ( intersect, union )
import Control.Arrow ( second, (***) )

--------------------------------------------------------------------------------

toyAlpha :: [ String ] -> Value -> Value
toyAlpha = alpha

toyAlphaEnv :: [ Pattern ] -> Env -> Env
toyAlphaEnv = flip ( foldr alphaEnv )  . ( patVars `concatMap` )

patVars :: Pattern -> [ String ]
patVars = map varName . Value.patVars

alphaEnv :: String -> Env -> Env
alphaEnv v = mapEnv ( succVar v ) ( succVar v ) ( succVar v )

alpha :: [ String ] -> Value -> Value
alpha pre ( Comp con mems )	= Comp con $ alpha pre `map` mems
alpha pre ( App fun arg )	= App ( alpha pre fun ) $ alpha pre arg
alpha pre ( Lambda ps expr )	= Lambda ( mapSuccVars dups ps ) $
	alpha ( pre `union` vars ) $ succVars dups expr
	where
	vars	= patVars `concatMap` ps
	dups	= pre `intersect` vars
alpha pre ( Case key sels )	= Case ( alpha pre key ) $ alphaC pre `map` sels
alpha pre ( Letin defs expr )	=
	Letin ( alphaDefs pre defs ) $ alpha newPre $ succVars dups expr
	where
	vars	= ( patVars . fst ) `concatMap` defs
	dups	= pre `intersect` vars
	newPre	= pre `union` vars
alpha pre ( Module defs )	= Module $ alphaDefs pre defs
alpha pre ( Let defs )		= Let $ second ( alpha pre ) `map` defs
alpha _ v			= v

alphaC :: [ String ] -> ( Pattern, Value ) -> ( Pattern, Value )
alphaC pre sel@( test, _ ) = second ( alpha newPre ) $ succVars dups sel
	where
	vars	= patVars test
	dups	= pre `intersect` vars
	newPre	= pre `union` vars

alphaDefs :: [ String ] -> [ ( Pattern, Value ) ] -> [ ( Pattern, Value ) ]
alphaDefs pre defs		=
	( second ( alpha newPre ) . succVars dups ) `map` defs
	where
	vars	= ( patVars . fst ) `concatMap` defs
	dups	= pre `intersect` vars
	newPre	= pre `union` vars

succVars :: Alpha sv => [ String ] -> sv -> sv
succVars = flip $ foldr succVar

mapSuccVars :: Alpha sv => [ String ] -> [ sv ] -> [ sv ]
mapSuccVars = map . succVars

class Alpha sv where
	succVar	:: String -> sv -> sv

instance Alpha Var where
	succVar x0 x1
		| x0 == varName x1	= V ( varName x1 ) $ varVol x1 + 1
		| otherwise			= x1

instance Alpha Pattern where
	succVar	= succVarPat . mkVar

instance Alpha Value where
	succVar	= setNextValue . mkVar

instance ( Alpha a, Alpha b ) => Alpha ( a, b ) where
	succVar vars	= succVar vars *** succVar vars

setNextValue :: Var -> Value -> Value
setNextValue v0 ( Lambda vs expr )	=
	Lambda ( succVarPat v0 `map` vs ) $ setNextValue v0 expr
setNextValue v0 ( Var v1 n )
	| varName v0 == v1		= Var v1 $ n + 1
setNextValue v0 ( App v1 v2 )		=
	App ( setNextValue v0 v1 ) ( setNextValue v0 v2 )
setNextValue v0 ( Comp con vs )	=
	Comp con $ setNextValue v0 `map` vs
setNextValue v0 ( Case v1 ps )		=
	Case ( setNextValue v0 v1 ) ( succVar ( varName v0 ) `map` ps )
setNextValue v0 ( Letin ps expr )	=
	Letin ( succVar ( varName v0 ) `map` ps ) $ setNextValue v0 expr
setNextValue _ v			= v

succVarPat :: Var -> Pattern -> Pattern
succVarPat v0 ( PatVar v1 n )
	| varName v0 == v1	= PatVar v1 $ n + 1
succVarPat v0 ( PatCon c pats )	= PatCon c $ succVarPat v0 `map` pats
succVarPat _ p		= p
