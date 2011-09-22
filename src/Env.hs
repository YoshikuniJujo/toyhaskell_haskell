{-# LANGUAGE TupleSections #-}

module Env (
	Env,
	initialize,
	setVars,
	setPat,
	setPats,
	getVal,
	getVars,
	mapEnv,
	Var( V ),
	succVar
) where

import Data.Maybe ( listToMaybe )
import Control.Arrow ( first )

data Var = V String Int deriving Eq

instance Show Var where
	show ( V v n ) = v ++ "~" ++ show n

succVar :: Var -> Var
succVar ( V v n ) = V v $ succ n

data Env p v = Env [ ( [ Var ], p, v ) ] deriving Show

empty :: Env p v
empty = Env [ ]

initialize :: ( String -> p ) -> [ ( String, v ) ] -> Env p v
initialize pv defs =
	setVars ( pv . \( V x _ ) -> x ) empty $ map ( first $ flip V 0 ) defs

setToEnv :: ( Var -> p ) -> Var -> v -> Env p v -> Env p v
setToEnv pv var val ( Env ps ) = Env $ ( [ var ], pv var, val ) : ps

setVars :: ( Var -> p ) -> Env p v -> [ ( Var, v ) ] -> Env p v
setVars pv = foldr $ uncurry ( setToEnv pv )

setPat :: ( p -> [ Var ] ) -> Env p v -> p -> v -> Env p v
setPat gpv ( Env env ) pat val = Env $ ( gpv pat, pat, val ) : env

setPats :: ( p -> [ Var ] ) -> Env p v -> [ ( p, v ) ] -> Env p v
setPats gpv = foldr $ uncurry ( flip . ( flip $ setPat gpv ) )

getVal :: ( v -> p -> Maybe [ ( Var, v ) ] ) -> ( v -> v ) ->
	Env p v -> Var -> Maybe v
getVal m f ( Env ps ) var = do
	( _, pat, val ) <- listToMaybe $ filter ( ( var `elem` ) . one ) ps
	m ( f val ) pat >>= lookup var
	where one ( x, _, _ ) = x

getVars :: Env p v -> [ String ]
getVars ( Env ps ) = ( \( vs, _, _ ) -> map ( \( V x _ ) -> x ) vs ) `concatMap` ps

mapEnv :: ( Var -> Var ) -> ( p -> p ) -> ( v -> v ) -> Env p v -> Env p v
mapEnv fs fp fv ( Env e ) = Env $ ( \( s, p, v ) -> ( map fs s, fp p, fv v ) ) `map` e
