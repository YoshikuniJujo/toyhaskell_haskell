{-# LANGUAGE TupleSections #-}

module Alpha ( alpha ) where

import Value
import Data.List
import Control.Arrow

alpha :: [ ( String, Int ) ] -> Value -> Value
alpha used = makeVar . alpha_ used

makeVar :: Value -> Value
makeVar ( Identifier var 0 )	= Identifier var 0
makeVar ( Identifier var n )	= Identifier ( var ++ "~" ++ show n ) n
makeVar ( Complex con vs )	= Complex con $ map makeVar vs
makeVar ( Apply v1 v2 )		= Apply ( makeVar v1 ) ( makeVar v2 )
makeVar ( Lambda ps v )		= Lambda ( map makeVarP ps ) ( makeVar v )
makeVar ( Case v ps )		= Case ( makeVar v ) ( map makeVarPair ps )
makeVar ( Letin ps v )		= Letin ( map makeVarPair ps ) ( makeVar v )
makeVar ( Let ps )		= Let ( map makeVarPair ps )
makeVar v			= v

makeVarPair :: ( Pattern, Value ) -> ( Pattern, Value )
makeVarPair ( p, v ) = ( makeVarP p, makeVar v )

makeVarP :: Pattern -> Pattern
makeVarP ( PatVar var 0 )	= PatVar var 0
makeVarP ( PatVar var n )	= PatVar ( var ++ "~" ++ show n ) n
makeVarP ( PatCon con pats )	= PatCon con ( map makeVarP pats )
makeVarP p			= p

alpha_ :: [ ( String, Int ) ] -> Value -> Value
alpha_ used ( Lambda vs body )	=
	Lambda ( map ( setNextsPat usedV ) vs ) $ alpha_ nused $
		setNextsValue usedV body
	where
	usedV	= map fst used `intersect` concatMap getPatVars vs
	nused	= updateUsed used ( concatMap getPatVars vs )
alpha_ used ( Apply v1 v2 )	=
	Apply ( alpha_ used v1 ) ( alpha_ used v2 )
alpha_ used ( Complex con vs )	= Complex con $ map ( alpha_ used ) vs
alpha_ used ( Case v ps )	=
	Case ( alpha_ used v ) $ map ( alphaCase used ) ps
alpha_ used ( Letin ps v )	=
	Letin ( map ( second ( alpha_ nused ) . setNextsPair usedV ) ps ) $
		alpha_ nused $ setNextsValue usedV v
	where
	usedV	= map fst used `intersect` concatMap getPatVars ( map fst ps )
	nused	= updateUsed used ( concatMap getPatVars $ map fst ps )
alpha_ used ( Let ps )		=
	Let $ map ( second ( alpha_ nused ) . setNextsPair usedV ) ps
	where
	usedV	= map fst used `intersect` concatMap getPatVars ( map fst ps )
	nused	= updateUsed used ( concatMap getPatVars $ map fst ps )
alpha_ _ v			= v

alphaCase :: [ ( String, Int ) ] -> ( Pattern, Value ) -> ( Pattern, Value )
alphaCase used ( pat, val )	=
	( setNextsPat usedV pat, alpha_ nused $ setNextsValue usedV val )
	where
	usedV	= map fst used `intersect` getPatVars pat
	nused	= updateUsed used ( getPatVars pat )

updateUsed :: [ ( String, Int ) ] -> [ String ] -> [ ( String, Int ) ]
updateUsed [ ] vars = map ( , 1 ) vars
updateUsed ( ( v, n ) : us ) vars
	| v `elem` vars	= ( v, n + 1 ) : updateUsed us ( filter ( /= v ) vars )
	| otherwise	= ( v, n ) : updateUsed us vars

setNextsValue :: [ String ] -> Value -> Value
setNextsValue = flip $ foldr setNextValue

setNextsPat :: [ String ] -> Pattern -> Pattern
setNextsPat = flip $ foldr setNextPat

setNextValue :: String -> Value -> Value
setNextValue v0 ( Lambda vs body )	=
	Lambda ( map ( setNextPat v0 ) vs ) $ setNextValue v0 body
setNextValue v0 ( Identifier v1 n )
	| v0 == v1			= Identifier v1 $ n + 1
setNextValue v0 ( Apply v1 v2 )		=
	Apply ( setNextValue v0 v1 ) ( setNextValue v0 v2 )
setNextValue v0 ( Complex con vs )	=
	Complex con $ map ( setNextValue v0 ) vs
setNextValue v0 ( Case v1 ps )		=
	Case ( setNextValue v0 v1 ) ( map ( setNextPair v0 ) ps )
setNextValue v0 ( Letin ps body )	=
	Letin ( map ( setNextPair v0 ) ps ) $ setNextValue v0 body
setNextValue _ v			= v

setNextsPair :: [ String ] -> ( Pattern, Value ) -> ( Pattern, Value )
setNextsPair = flip $ foldr setNextPair

setNextPair :: String -> ( Pattern, Value ) -> ( Pattern, Value )
setNextPair v0 ( p, v ) = ( setNextPat v0 p, setNextValue v0 v )

setNextPat :: String -> Pattern -> Pattern
setNextPat v0 ( PatVar v1 n )
	| v0 == v1	= PatVar v1 $ n + 1
setNextPat v0 ( PatCon c pats )	= PatCon c $ map ( setNextPat v0 ) pats
setNextPat _ p		= p
