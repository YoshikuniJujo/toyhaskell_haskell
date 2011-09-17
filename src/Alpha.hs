{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Alpha ( toyAlpha, toyAlphaEnv ) where

import Value (
	Value( Var, Comp, App, Lambda, Case, Letin, Let, Module ),
	Pattern( PatVar, PatCon ), patVars, Env, mapEnv, Var )
import Data.List ( intersect, union )
import Control.Arrow ( first, second, (***) )

--------------------------------------------------------------------------------

toyAlpha :: [ Var ] -> Value -> Value
toyAlpha pre = {- mkVar . -} alpha $ map fst pre

toyAlphaEnv :: [ Pattern ] -> Env -> Env
toyAlphaEnv ps = flip ( foldr alphaEnv )  $ patVars `concatMap` ps

alphaEnv :: Var -> Env -> Env
alphaEnv x0 = mapEnv ( succVarStrInt x0 ) ( {- mkVar . -} succVar x0 ) ( {- mkVar . -} succVar x0 )

alpha :: [ String ] -> Value -> Value
alpha pre ( Comp con mems )	= Comp con $ alpha pre `map` mems
alpha pre ( App fun arg )	= App ( alpha pre fun ) $ alpha pre arg
alpha pre ( Lambda ps expr )	= Lambda ( mapSuccVars ( map ( , 0 ) dups ) ps ) $
	alpha ( pre `union` vars ) $ succVars ( map ( , 0 ) dups ) expr
	where
	vars	= map fst $ patVars `concatMap` ps
	dups	= pre `intersect` vars
alpha pre ( Case key sels )	= Case ( alpha pre key ) $ alphaC pre `map` sels
alpha pre ( Letin defs expr )	=
	Letin ( alphaDefs pre defs ) $ alpha newPre $ succVars ( map ( , 0 ) dups ) expr
	where
	vars	= map fst $ ( patVars . fst ) `concatMap` defs
	dups	= pre `intersect` vars
	newPre	= pre `union` vars
alpha pre ( Module defs )	= Module $ alphaDefs pre defs
alpha pre ( Let defs )		= Let $ second ( alpha pre ) `map` defs
alpha _ v			= v

alphaC :: [ String ] -> ( Pattern, Value ) -> ( Pattern, Value )
alphaC pre sel@( test, _ ) = second ( alpha newPre ) $ succVars ( map ( , 0 ) dups ) sel
	where
	vars	= map fst $ patVars test
	dups	= pre `intersect` vars
	newPre	= pre `union` vars

alphaDefs :: [ String ] -> [ ( Pattern, Value ) ] -> [ ( Pattern, Value ) ]
alphaDefs pre defs		=
	( second ( alpha newPre ) . succVars ( map ( , 0 ) dups ) ) `map` defs
	where
	vars	= map fst $ ( patVars . fst ) `concatMap` defs
	dups	= pre `intersect` vars
	newPre	= pre `union` vars

succVars :: Alpha sv => [ Var ] -> sv -> sv
succVars = flip $ foldr succVar

mapSuccVars :: Alpha sv => [ Var ] -> [ sv ] -> [ sv ]
mapSuccVars = map . succVars

class Alpha sv where
	succVar	:: Var -> sv -> sv
	mkVar	:: sv -> sv

instance Alpha ( String, Int ) where
	succVar ( x0, _ ) ( x1, n )
		| x0 == x1	= ( x1, n + 1 )
		| otherwise	= ( x1, n )

succVarStrInt ( x0, _ ) ( x1, n )
	| x0 == x1	= ( x1, n + 1 )
	| otherwise	= ( x1, n )

{-
instance Alpha String where
	succVar ( x0, _ ) x1
		| x0 == x1			= x1 ++ "~1"
		| x0 == takeWhile ( /= '~' ) x1	= x0 ++ "~" ++
			show ( 1 + ( read $ tail $ dropWhile ( /= '~' ) x1 :: Int ) )
		| otherwise			= x1
	mkVar		= id
-}

instance Alpha Pattern where
	succVar	= succVarPat
	mkVar	= mkVarPat

instance Alpha Value where
	succVar	= setNextValue
	mkVar	= mkVarVal

instance ( Alpha a, Alpha b ) => Alpha ( a, b ) where
	succVar vars	= succVar vars *** succVar vars
	mkVar		= mkVar *** mkVar

setNextValue :: Var -> Value -> Value
setNextValue v0 ( Lambda vs expr )	=
	Lambda ( succVarPat v0 `map` vs ) $ setNextValue v0 expr
setNextValue ( v0, _ ) ( Var v1 n )
	| v0 == v1			= Var v1 $ n + 1
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
succVarPat ( v0, _ ) ( PatVar v1 n )
	| v0 == v1	= PatVar v1 $ n + 1
succVarPat v0 ( PatCon c pats )	= PatCon c $ succVarPat v0 `map` pats
succVarPat _ p		= p

mkVarVal :: Value -> Value
mkVarVal ( Var var 0 )		= Var var 0
mkVarVal ( Var var n )		= Var ( var ++ "~" ++ show n ) n
mkVarVal ( Comp con mems )	= Comp con $ mkVar `map` mems
mkVarVal ( App fun arg )	= App ( mkVar fun ) $ mkVar arg
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
