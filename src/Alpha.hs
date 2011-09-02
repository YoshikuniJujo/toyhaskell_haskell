{-# LANGUAGE TupleSections #-}

module Alpha ( alpha ) where

import Value ( Value( .. ), Pattern( .. ), getPatVars )
import Data.List ( intersect, union )
import Control.Arrow ( second, (***) )

--------------------------------------------------------------------------------

alpha :: Value -> Value
alpha = mkVar . alpha_ [ ]

alpha_ :: [ String ] -> Value -> Value
alpha_ pre ( Complex con mems )	= Complex con $ alpha_ pre `map` mems
alpha_ pre ( Apply fun arg )	= Apply ( alpha_ pre fun ) $ alpha_ pre arg
alpha_ pre ( Lambda args body )	= Lambda ( mapSuccVars dups args ) $
	alpha_ ( pre `union` vars ) $ succVars dups body
	where
	vars	= getPatVars `concatMap` args
	dups	= pre `intersect` vars
alpha_ pre ( Case val bodies )	=
	Case ( alpha_ pre val ) $ alphaPV pre `map` bodies
alpha_ pre ( Letin defs body )	=
	Letin ( map ( second ( alpha_ newPre ) . succVars dups ) defs ) $
		alpha_ newPre $ succVars dups body
--	Letin ( alphaPV pre `map` defs ) $ alpha_ newPre $ succVars dups body
	where
	vars	= ( getPatVars . fst ) `concatMap` defs
	dups	= pre `intersect` vars
	newPre	= pre `union` vars
alpha_ pre ( Let defs )		=
	Let $ ( second ( alpha_ $ pre `union` vars ) .
		succVars ( pre `intersect` vars ) ) `map` defs
	where
	vars	= ( getPatVars . fst ) `concatMap` defs
	dups	= pre `intersect` vars
	newPre	= pre `union` vars
-- alpha_ pre ( Let defs )		= Let $ alphaPV pre `map` defs
alpha_ _ v			= v

alphaPV :: [ String ] -> ( Pattern, Value ) -> ( Pattern, Value )
alphaPV pre body@( pat, _ )	= second ( alpha_ $ pre `union` vars ) $
	succVars ( pre `intersect` vars ) body
	where vars = getPatVars pat

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
setNextValue v0 ( Lambda vs body )	=
	Lambda ( succVarPat v0 `map` vs ) $ setNextValue v0 body
setNextValue v0 ( Identifier v1 n )
	| v0 == v1			= Identifier v1 $ n + 1
setNextValue v0 ( Apply v1 v2 )		=
	Apply ( setNextValue v0 v1 ) ( setNextValue v0 v2 )
setNextValue v0 ( Complex con vs )	=
	Complex con $ setNextValue v0 `map` vs
setNextValue v0 ( Case v1 ps )		=
	Case ( setNextValue v0 v1 ) ( succVar v0 `map` ps )
setNextValue v0 ( Letin ps body )	=
	Letin ( succVar v0 `map` ps ) $ setNextValue v0 body
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
mkVarVal ( Apply fun arg )		= Apply ( mkVar fun ) $ mkVar arg
mkVarVal ( Lambda args body )	= Lambda ( mkVar `map` args ) $ mkVar body
mkVarVal ( Case val bodies )	= Case ( mkVar val ) $ mkVar `map` bodies
mkVarVal ( Letin defs body )	= Letin ( mkVar `map` defs ) $ mkVar body
mkVarVal ( Let defs )		= Let $ mkVar `map` defs
mkVarVal val			= val

mkVarPat :: Pattern -> Pattern
mkVarPat ( PatVar var 0 )	= PatVar var 0
mkVarPat ( PatVar var n )	= PatVar ( var ++ "~" ++ show n ) n
mkVarPat ( PatCon con pats )	= PatCon con $ mkVarPat `map` pats
mkVarPat p			= p
