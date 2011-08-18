module MainTools (
	mainGen
) where

import Parser ( toyParse )
import Eval ( eval, initEnv )
import Value ( Value( .. ), Env, showValue, addEnvs, setPatToEnv,
	emptyEnv )
import Interact ( runLoop )
import Data.List ( isPrefixOf )
import Data.Char ( isSpace )
import Control.Monad ( foldM )

mainGen :: [ String ] -> IO ()
mainGen args = do
	let ( expr, fns ) = getExpArgs args
	env0 <-  foldM ( flip ( runCmd .  ( "load " ++ ) ) ) initEnv fns
	case expr of
		Nothing -> runLoop "testLexer" env0 $ \env input -> case input of
			':' : cmd	-> runCmd cmd env
			_		-> case eval env $ toyParse input of
				Let ps	-> return $ ( foldr ( uncurry setPatToEnv )
					emptyEnv ps ) `addEnvs` env
				ret	-> showValue ret >> return env
		Just e -> showValue $ eval env0 $ toyParse e

runCmd :: String -> Env -> IO Env
runCmd cmd env
	| "load" `isPrefixOf` cmd	= do
		let fn = dropWhile isSpace $ drop 4 cmd
		cnt <- readFile fn
		case eval env $ toyParse ( "let\n" ++ cnt ) of
			Let ps	-> return $ ( foldr ( uncurry setPatToEnv ) emptyEnv $
					ps ) `addEnvs` env
			bad	-> error $ show bad
	| otherwise			= return env

getExpArgs :: [ String ] -> ( Maybe String, [ String ] )
getExpArgs ( "-e" : expr : rest ) = ( Just expr, rest )
getExpArgs rest = ( Nothing, rest )
