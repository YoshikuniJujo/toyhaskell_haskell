{-# LANGUAGE TupleSections #-}

module Alpha ( toyAlpha, toyAlphaEnv ) where

import Value (
	Value( Identifier, Complex, Apply, Lambda, Case, Letin, Let, Module ),
	Pattern( PatVar, PatCon ), getPatVars, mapEnv, Env )
import Data.List ( intersect, union )
import Control.Arrow ( second, (***) )

--------------------------------------------------------------------------------

toyAlpha :: [ String ] -> Value -> Value
toyAlpha pre = mkVar . alpha pre

getVars :: Pattern -> [ String ]
getVars = getPatVars

toyAlphaEnv :: [ Pattern ] -> Env -> Env
toyAlphaEnv ps = alphaEnvS $ getVars `concatMap` ps

alphaEnvS :: [ String ] -> Env -> Env
alphaEnvS = flip $ foldr alphaEnv_

alphaEnv_ :: String -> Env -> Env
alphaEnv_ x0 = mapEnv fs ( succVar x0 ) ( succVar x0 )
	where
	fs x	| x == x0	= x ++ "~1"
		| otherwise	= x

alpha :: [ String ] -> Value -> Value
alpha pre ( Complex con mems )	= Complex con $ alpha pre `map` mems
alpha pre ( Apply fun arg )	= Apply ( alpha pre fun ) $ alpha pre arg
alpha pre ( Lambda ps expr )	= Lambda ( mapSuccVars dups ps ) $
	alpha ( pre `union` vars ) $ succVars dups expr
	where
	vars	= getVars `concatMap` ps
	dups	= pre `intersect` vars
alpha pre ( Case key sels )	= Case ( alpha pre key ) $ alphaC pre `map` sels
alpha pre ( Letin defs expr )	=
	Letin ( alphaDefs pre defs ) $ alpha newPre $ succVars dups expr
	where
	vars	= ( getVars . fst ) `concatMap` defs
	dups	= pre `intersect` vars
	newPre	= pre `union` vars
alpha pre ( Module defs )	= Module $ alphaDefs pre defs
alpha pre ( Let defs )		= Let $ second ( alpha pre ) `map` defs
alpha _ v			= v

alphaC :: [ String ] -> ( Pattern, Value ) -> ( Pattern, Value )
alphaC pre sel@( test, _ ) = second ( alpha newPre ) $ succVars dups sel
	where
	vars	= getVars test
	dups	= pre `intersect` vars
	newPre	= pre `union` vars

alphaDefs :: [ String ] -> [ ( Pattern, Value ) ] -> [ ( Pattern, Value ) ]
alphaDefs pre defs		=
	( second ( alpha newPre ) . succVars dups ) `map` defs
	where
	vars	= ( getVars . fst ) `concatMap` defs
	dups	= pre `intersect` vars
	newPre	= pre `union` vars

succVars :: Alpha sv => [ String ] -> sv -> sv
succVars = flip $ foldr succVar

mapSuccVars :: Alpha sv => [ String ] -> [ sv ] -> [ sv ]
mapSuccVars = map . succVars

class Alpha sv where
	succVar	:: String -> sv -> sv
	mkVar	:: sv -> sv

instance Alpha Pattern where
	succVar	= succVarPat
	mkVar	= mkVarPat

instance Alpha Value where
	succVar	= setNextValue
	mkVar	= mkVarVal

instance ( Alpha a, Alpha b ) => Alpha ( a, b ) where
	succVar vars	= succVar vars *** succVar vars
	mkVar		= mkVar *** mkVar

setNextValue :: String -> Value -> Value
setNextValue v0 ( Lambda vs expr )	=
	Lambda ( succVarPat v0 `map` vs ) $ setNextValue v0 expr
setNextValue v0 ( Identifier v1 n )
	| v0 == v1			= Identifier v1 $ n + 1
setNextValue v0 ( Apply v1 v2 )		=
	Apply ( setNextValue v0 v1 ) ( setNextValue v0 v2 )
setNextValue v0 ( Complex con vs )	=
	Complex con $ setNextValue v0 `map` vs
setNextValue v0 ( Case v1 ps )		=
	Case ( setNextValue v0 v1 ) ( succVar v0 `map` ps )
setNextValue v0 ( Letin ps expr )	=
	Letin ( succVar v0 `map` ps ) $ setNextValue v0 expr
setNextValue _ v			= v

succVarPat :: String -> Pattern -> Pattern
succVarPat v0 ( PatVar v1 n )
	| v0 == v1	= PatVar v1 $ n + 1
succVarPat v0 ( PatCon c pats )	= PatCon c $ succVarPat v0 `map` pats
succVarPat _ p		= p

mkVarVal :: Value -> Value
mkVarVal ( Identifier var 0 )	= Identifier var 0
mkVarVal ( Identifier var n )	= Identifier ( var ++ "~" ++ show n ) n
mkVarVal ( Complex con mems )	= Complex con $ mkVar `map` mems
mkVarVal ( Apply fun arg )	= Apply ( mkVar fun ) $ mkVar arg
mkVarVal ( Lambda ps expr )	= Lambda ( mkVar `map` ps ) $ mkVar expr
mkVarVal ( Case key sels )	= Case ( mkVar key ) $ mkVar `map` sels
mkVarVal ( Letin defs expr )	= Letin ( mkVar `map` defs ) $ mkVar expr
mkVarVal ( Let defs )		= Let $ mkVar `map` defs
mkVarVal ( Module defs )	= Module $ mkVar `map` defs
mkVarVal val			= val

mkVarPat :: Pattern -> Pattern
mkVarPat ( PatVar var 0 )	= PatVar var 0
mkVarPat ( PatVar var n )	= PatVar ( var ++ "~" ++ show n ) n
mkVarPat ( PatCon con pats )	= PatCon con $ mkVarPat `map` pats
mkVarPat p			= p
