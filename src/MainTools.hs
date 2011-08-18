module MainTools (
	mainGen
) where

import Interact ( runLoop )
import Value ( Value( .. ), showValue, Env, setPatsToEnv )
import Parser ( toyParse )
import Eval ( eval, initEnv )

import System.Console.GetOpt (
	getOpt, ArgOrder( .. ), OptDescr( .. ), ArgDescr( .. ) )
import Control.Monad ( foldM )
import Data.List ( isPrefixOf )
import Data.Char ( isSpace )

mainGen :: [ String ] -> IO ()
mainGen args = do
	let ( expr, fns, _ ) = getOpt RequireOrder options args
	e0 <- foldM loadFile initEnv fns
	withSingle expr ( showValue . eval e0 . toyParse ) $
		runLoop "toyhaskell" e0 $ \e inp -> case inp of
			':' : cmd	-> runCmd cmd e
			_		-> case eval e $ toyParse inp of
				Let ps	-> return $ setPatsToEnv ps e
				ret	-> showValue ret >> return e

options :: [ OptDescr String ]
options = [
	Option [ 'e' ] [ ] ( ReqArg id "haskell expression" ) "run expression"
 ]

runCmd :: String -> Env -> IO Env
runCmd "quit" env			= return env
runCmd "q" env				= return env
runCmd cmd env
	| "load" `isPrefixOf` cmd	= do
		let fn = dropWhile isSpace $ drop 4 cmd
		loadFile env fn
	| otherwise			= do
		putStrLn $ "unknown command ':" ++ cmd  ++ "'"
		return env

loadFile :: Env -> FilePath -> IO Env
loadFile env fn = do
	cnt <- readFile fn
	case eval env $ toyParse ( "let\n" ++ cnt ) of
		Let ps	-> return $ setPatsToEnv ps env
		bad	-> error $ show bad

withSingle :: [ a ] -> ( a -> b ) -> b -> b
withSingle [ ] _ y	= y
withSingle [ x ] f _	= f x
withSingle _ _ _	= error "Not single list."
