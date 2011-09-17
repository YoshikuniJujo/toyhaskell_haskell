module Env (
	Env,
	initEnv,
	setsToEnv,
	setPatToEnv,
	setPatsToEnv,
	getFromEnv,
	getVarsEnv,
	addEnvs,
	mapEnv,
	Var
) where

import Data.Maybe ( listToMaybe )

type Var = ( String, Int )

data Env p v = Env [ ( [ Var ], p, v ) ] deriving Show

emptyEnv :: Env p v
emptyEnv = Env [ ]

initEnv :: ( Var -> p ) -> [ ( Var, v ) ] -> Env p v
initEnv pv defs = setsToEnv pv defs emptyEnv

setToEnv :: ( Var -> p ) -> Var -> v -> Env p v -> Env p v
setToEnv pv var val ( Env ps ) = Env $ ( [ var ], pv var, val ) : ps

setsToEnv :: ( Var -> p ) -> [ ( Var, v ) ] -> Env p v -> Env p v
setsToEnv pv = flip $ foldr $ uncurry ( setToEnv pv )

setPatToEnv :: ( p -> [ Var ] ) -> p -> v -> Env p v -> Env p v
setPatToEnv gpv pat val ( Env env ) = Env $ ( gpv pat, pat, val ) : env

setPatsToEnv :: ( p -> [ Var ] ) -> [ ( p, v ) ] -> Env p v -> Env p v
setPatsToEnv gpv = flip $ foldr $ uncurry ( setPatToEnv gpv )

getFromEnv :: ( v -> p -> Maybe [ ( Var, v ) ] ) -> ( v -> v ) -> Var ->
	Env p v -> Maybe v
getFromEnv m f var ( Env ps ) = do
	( _, pat, val ) <- listToMaybe $ filter ( ( var `elem` ) . one ) ps
	m ( f val ) pat >>= lookup var
	where one ( x, _, _ ) = x

getVarsEnv :: Env p v -> [ Var ]
getVarsEnv ( Env ps ) = ( \( vs, _, _ ) -> vs ) `concatMap` ps

addEnvs :: Env p v -> Env p v -> Env p v
addEnvs ( Env ps1 ) ( Env ps2 ) = Env $ ps1 ++ ps2

mapEnv :: ( Var -> Var ) -> ( p -> p ) -> ( v -> v ) -> Env p v -> Env p v
mapEnv fs fp fv ( Env e ) = Env $ ( \( s, p, v ) -> ( map fs s, fp p, fv v ) ) `map` e
