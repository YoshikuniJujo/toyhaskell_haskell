module Env (
	Env,
	emptyEnv,
	setsToEnv,
	setPatToEnv,
	setPatsToEnv,
	getFromEnv,
	addEnvs
) where

import Data.Maybe

data Env p v = Env [ ( [ String ], p, v ) ]

emptyEnv :: Env p v
emptyEnv = Env [ ]

setToEnv :: ( String -> p ) -> String -> v -> Env p v -> Env p v
setToEnv pv var val ( Env ps ) =
	Env $ ( [ var ], pv var, val ) : ps

setsToEnv :: ( String -> p ) -> [ ( String, v ) ] -> Env p v -> Env p v
setsToEnv pv = flip $ foldr $ uncurry ( setToEnv pv )

setPatToEnv :: ( p -> [ String ] ) -> p -> v -> Env p v -> Env p v
setPatToEnv gpv pat val ( Env env ) = Env $ ( vars, pat, val ) : env
	where vars = gpv pat

setPatsToEnv :: ( p -> [ String ] ) -> [ ( p, v ) ] -> Env p v -> Env p v
setPatsToEnv gpv = flip $ foldr $ uncurry ( setPatToEnv gpv )

getFromEnv :: ( v -> p -> Maybe [ ( String, v ) ] ) -> ( v -> v ) -> String ->
	Env p v -> Maybe v
getFromEnv m f var ( Env ps ) = do
	( _, pat, val ) <- usePat
	m ( f val ) pat >>= lookup var
	where
	one ( x, _, _ ) = x
	usePat = listToMaybe $ filter ( ( var `elem` ) . one ) ps

addEnvs :: Env p v -> Env p v -> Env p v
addEnvs ( Env ps1 ) ( Env ps2 ) = Env $ ps1 ++ ps2
