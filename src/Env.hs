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
	succVar,
	vName
) where

import Data.Maybe ( listToMaybe )

data Var = V { vName :: String, _varVol :: Int } deriving Eq

instance Show Var where
	show ( V v n ) = v ++ "~" ++ show n

succVar :: Var -> Var
succVar ( V v n ) = V v $ succ n

data Env p v = Env [ ( [ Var ], p, v ) ] deriving Show

emptyEnv :: Env p v
emptyEnv = Env [ ]

initialize :: ( Var -> p ) -> [ ( Var, v ) ] -> Env p v
initialize pv defs = setVars pv emptyEnv defs

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
getVars ( Env ps ) = ( \( vs, _, _ ) -> map vName vs ) `concatMap` ps

mapEnv :: ( Var -> Var ) -> ( p -> p ) -> ( v -> v ) -> Env p v -> Env p v
mapEnv fs fp fv ( Env e ) = Env $ ( \( s, p, v ) -> ( map fs s, fp p, fv v ) ) `map` e
