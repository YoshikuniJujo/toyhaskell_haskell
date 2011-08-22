module MainTools (
	mainGen
) where

import Interact ( runLoop )
import Eval ( eval, initEnv )
import Parser ( toyParse )
import Value ( Value( .. ), showValue, Env, setPatsToEnv )

import System.Console.GetOpt (
	getOpt, ArgOrder( .. ), OptDescr( .. ), ArgDescr( .. ) )
import Control.Monad ( foldM )
import Data.List ( isPrefixOf )
import Data.Char ( isSpace )

--------------------------------------------------------------------------------

mainGen :: [ String ] -> [ String ] -> IO ()
mainGen args _ = do
	let ( opts, fns, errs ) = getOpt RequireOrder options args
	mapM_ putStr errs
	env0 <- foldM loadFile initEnv fns
	withSingle opts ( showValue . eval env0 . toyParse "" . getExpr ) $
		runLoop "toyhaskell" env0 $ \env inp -> case inp of
			':' : cmd	-> runCmd cmd env
			_		-> case eval env $
						toyParse "<interactive>" inp of
				Let ps	-> return $ setPatsToEnv ps env
				ret	-> showValue ret >> return env

data Option = Expr { getExpr :: String }

options :: [ OptDescr Option ]
options = [
	Option "e" [ ] ( ReqArg Expr "haskell expression" ) "run expression"
 ]

runCmd :: String -> Env -> IO Env
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
	case eval env $ toyParse fn ( "let\n" ++ cnt ) of
		Let ps	-> return $ setPatsToEnv ps env
		bad	-> error $ show bad

withSingle :: [ a ] -> ( a -> b ) -> b -> b
withSingle [ ] _ y	= y
withSingle [ x ] f _	= f x
withSingle _ _ _	= error "Not single list."
