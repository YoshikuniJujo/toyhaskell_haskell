module Env (
	Env,
	initEnv,
	setsToEnv,
	setPatToEnv,
	setPatsToEnv,
	getFromEnv,
	getVarsEnv,
	addEnvs,
	mapEnv
) where

import Data.Maybe ( listToMaybe )

data Env p v = Env [ ( [ String ], p, v ) ]

emptyEnv :: Env p v
emptyEnv = Env [ ]

initEnv :: ( String -> p ) -> [ ( String, v ) ] -> Env p v
initEnv pv defs = setsToEnv pv defs emptyEnv

setToEnv :: ( String -> p ) -> String -> v -> Env p v -> Env p v
setToEnv pv var val ( Env ps ) = Env $ ( [ var ], pv var, val ) : ps

setsToEnv :: ( String -> p ) -> [ ( String, v ) ] -> Env p v -> Env p v
setsToEnv pv = flip $ foldr $ uncurry ( setToEnv pv )

setPatToEnv :: ( p -> [ String ] ) -> p -> v -> Env p v -> Env p v
setPatToEnv gpv pat val ( Env env ) = Env $ ( gpv pat, pat, val ) : env

setPatsToEnv :: ( p -> [ String ] ) -> [ ( p, v ) ] -> Env p v -> Env p v
setPatsToEnv gpv = flip $ foldr $ uncurry ( setPatToEnv gpv )

getFromEnv :: ( v -> p -> Maybe [ ( String, v ) ] ) -> ( v -> v ) -> String ->
	Env p v -> Maybe v
getFromEnv m f var ( Env ps ) = do
	( _, pat, val ) <- listToMaybe $ filter ( ( var `elem` ) . one ) ps
	m ( f val ) pat >>= lookup var
	where one ( x, _, _ ) = x

getVarsEnv :: Env p v -> [ String ]
getVarsEnv ( Env ps ) = ( \( vs, _, _ ) -> vs ) `concatMap` ps

addEnvs :: Env p v -> Env p v -> Env p v
addEnvs ( Env ps1 ) ( Env ps2 ) = Env $ ps1 ++ ps2

mapEnv :: ( String -> String ) -> ( p -> p ) -> ( v -> v ) -> Env p v -> Env p v
mapEnv fs fp fv ( Env e ) = Env $ ( \( s, p, v ) -> ( map fs s, fp p, fv v ) ) `map` e
