module Env where

import Control.Monad
import Data.Maybe
import Types

emptyEnv :: Env
emptyEnv = Env [ ]

setToEnv :: String -> Value -> Env -> Env
setToEnv var val ( Env ps ) =
	Env $ ( [ var ], PatVar var, val ) : ps

setsToEnv :: [ ( String, Value ) ] -> Env -> Env
setsToEnv = flip $ foldr $ uncurry setToEnv

setPatToEnv :: Pattern -> Value -> Env -> Env
setPatToEnv pat val ( Env env ) = Env $ ( vars, pat, val ) : env
	where vars = getPatVars pat

setPatsToEnv :: [ ( Pattern, Value ) ] -> Env -> Env
setPatsToEnv = flip $ foldr $ uncurry setPatToEnv

getPatVars :: Pattern -> [ String ]
getPatVars ( PatConst _ pats ) = concatMap getPatVars pats
getPatVars ( PatVar var ) = [ var ]
getPatVars _ = [ ]

getFromEnv :: ( Value -> Value ) -> String -> Env -> Maybe Value
getFromEnv f var ( Env ps ) = do
	( _, pat, val ) <- usePat
	match ( f val ) pat >>= lookup var
	where
	one ( x, _, _ ) = x
	usePat = listToMaybe $ filter ( ( var `elem` ) . one ) ps

addEnvs :: Env -> Env -> Env
addEnvs ( Env ps1 ) ( Env ps2 ) = Env $ ps1 ++ ps2
