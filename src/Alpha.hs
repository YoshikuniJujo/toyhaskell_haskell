module Alpha ( toyAlpha, toyAlphaEnv ) where

import Value (
	Value( Var, Comp, App, Lambda, Case, Letin, Let, Module ),
	Pattern( PatVar, PatCon ), patVars, Env, mapEnv,
	Var( V, varName, varVol ), mkVar )
import Data.List ( intersect, union )
import Control.Arrow ( second, (***) )

--------------------------------------------------------------------------------

toyAlpha :: [ Var ] -> Value -> Value
toyAlpha pre = alpha $ map varName pre

toyAlphaEnv :: [ Pattern ] -> Env -> Env
toyAlphaEnv ps = flip ( foldr alphaEnv )  $ patVars `concatMap` ps

alphaEnv :: Var -> Env -> Env
alphaEnv v = mapEnv ( succVar v ) ( succVar v ) ( succVar v )

alpha :: [ String ] -> Value -> Value
alpha pre ( Comp con mems )	= Comp con $ alpha pre `map` mems
alpha pre ( App fun arg )	= App ( alpha pre fun ) $ alpha pre arg
alpha pre ( Lambda ps expr )	= Lambda ( mapSuccVars ( map mkVar dups ) ps ) $
	alpha ( pre `union` vars ) $ succVars ( map mkVar dups ) expr
	where
	vars	= map varName $ patVars `concatMap` ps
	dups	= pre `intersect` vars
alpha pre ( Case key sels )	= Case ( alpha pre key ) $ alphaC pre `map` sels
alpha pre ( Letin defs expr )	=
	Letin ( alphaDefs pre defs ) $ alpha newPre $ succVars ( map mkVar dups ) expr
	where
	vars	= map varName $ ( patVars . fst ) `concatMap` defs
	dups	= pre `intersect` vars
	newPre	= pre `union` vars
alpha pre ( Module defs )	= Module $ alphaDefs pre defs
alpha pre ( Let defs )		= Let $ second ( alpha pre ) `map` defs
alpha _ v			= v

alphaC :: [ String ] -> ( Pattern, Value ) -> ( Pattern, Value )
alphaC pre sel@( test, _ ) = second ( alpha newPre ) $ succVars ( map mkVar dups ) sel
	where
	vars	= map varName $ patVars test
	dups	= pre `intersect` vars
	newPre	= pre `union` vars

alphaDefs :: [ String ] -> [ ( Pattern, Value ) ] -> [ ( Pattern, Value ) ]
alphaDefs pre defs		=
	( second ( alpha newPre ) . succVars ( map mkVar dups ) ) `map` defs
	where
	vars	= map varName $ ( patVars . fst ) `concatMap` defs
	dups	= pre `intersect` vars
	newPre	= pre `union` vars

succVars :: Alpha sv => [ Var ] -> sv -> sv
succVars = flip $ foldr succVar

mapSuccVars :: Alpha sv => [ Var ] -> [ sv ] -> [ sv ]
mapSuccVars = map . succVars

class Alpha sv where
	succVar	:: Var -> sv -> sv

instance Alpha Var where
	succVar x0 x1
		| varName x0 == varName x1	= V ( varName x1 ) $ varVol x1 + 1
		| otherwise			= x1

instance Alpha Pattern where
	succVar	= succVarPat

instance Alpha Value where
	succVar	= setNextValue

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
	Case ( setNextValue v0 v1 ) ( succVar v0 `map` ps )
setNextValue v0 ( Letin ps expr )	=
	Letin ( succVar v0 `map` ps ) $ setNextValue v0 expr
setNextValue _ v			= v

succVarPat :: Var -> Pattern -> Pattern
succVarPat v0 ( PatVar v1 n )
	| varName v0 == v1	= PatVar v1 $ n + 1
succVarPat v0 ( PatCon c pats )	= PatCon c $ succVarPat v0 `map` pats
succVarPat _ p		= p
