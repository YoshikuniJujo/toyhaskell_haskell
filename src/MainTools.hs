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

import Paths_toyhaskell

--------------------------------------------------------------------------------

mainGen :: [ String ] -> [ String ] -> IO ()
mainGen args _ = do
	let ( opts, fns, errs ) = getOpt RequireOrder options args
	mapM_ putStr errs
	opLstFile <- if null ( filter isOpTable opts )
		then getDataFileName "operator-table.lst"
		else return $ getOpTable $ head $ filter isOpTable opts
	opLst <- readFile opLstFile -- getDataFileName "operator-table.lst" >>= readFile
	env0 <- foldM ( loadFile opLst ) initEnv fns
	withSingle ( filter isExpr opts )
		( showValue . eval env0 . toyParse opLst "" . getExpr ) $
		runLoop "toyhaskell" env0 $ \env inp -> case inp of
			':' : cmd	-> runCmd opLst cmd env
			_		-> case eval env $
						toyParse opLst "<interactive>" inp of
				Let ps	-> return $ setPatsToEnv ps env
				ret	-> showValue ret >> return env

data Option = Expr { getExpr :: String } | OpTable { getOpTable :: String }

isExpr :: Option -> Bool
isExpr ( Expr _ )	= True
isExpr _		= False
isOpTable ( OpTable _ )	= True
isOpTable _		= False

options :: [ OptDescr Option ]
options = [
	Option "e" [ ] ( ReqArg Expr "haskell expression" ) "run expression",
	Option "" [ "op-table" ] ( ReqArg OpTable "operation table path" )
	"set operation table"
 ]

runCmd :: String -> String -> Env -> IO Env
runCmd opLst cmd env
	| "load" `isPrefixOf` cmd	= do
		let fn = dropWhile isSpace $ drop 4 cmd
		loadFile opLst env fn
	| otherwise			= do
		putStrLn $ "unknown command ':" ++ cmd  ++ "'"
		return env

loadFile :: String -> Env -> FilePath -> IO Env
loadFile opLst env fn = do
	cnt <- readFile fn
	case eval env $ toyParse opLst fn ( "let\n" ++ cnt ) of
		Let ps	-> return $ setPatsToEnv ps env
		bad	-> error $ show bad

withSingle :: [ a ] -> ( a -> b ) -> b -> b
withSingle [ ] _ y	= y
withSingle [ x ] f _	= f x
withSingle _ _ _	= error "Not single list."
