module Main where

import MainTools ( mainGen )
import ToyHaskell
import Test.HUnit
import Alpha
import Parser
import Value

interpret :: String -> String -> IO String
interpret fn expr = mainGen [ "-e", expr, fn ] [ ]

testEnv :: Env
testEnv = initialize [ ( "x", Integer 8 ), ( "y", Var "x" 0 ) ]

testMain :: [ Test ]
testMain = [
	( ~? "eight" ) $ ( == "8\n" ) `fmap` interpret "./examples/eight.hs" "eight",
	( ~? "ret" ) $ ( == "20\n" ) `fmap` interpret "./examples/scope1.hs" "ret"
 ]

testEval :: [ Test ]
testEval = [
	( 8 :: Integer )	~?= eval primitives "8",
	'h'			~?= eval primitives "'h'",
	"hello"			~?= eval primitives "\"hello\"",
	True			~?= eval primitives "8 == 8",
	False			~?= eval primitives "8 == 3",
	[ ( 1 :: Integer ), 2, 3 ]
				~?= eval primitives "[ 3 - 2, 2, 1 + 2 ]"
 ]

testAlpha :: [ Test ]
testAlpha = [
	show ( alpha [ ] $ toyParse "\\x y z -> \\x z -> \\x -> x y z" ) ~?=
		"( \\x y z -> ( \\x~1 z~1 -> ( \\x~2 -> ( ( x~2 y ) z~1 ) ) ) )",
	show ( alpha [ ] $ toyParse
		"let x = 3; y = 2; z = 1; in \\x z -> \\x -> x y z" ) ~?=
		"let z = 1; y = 2; x = 3; in " ++
		"( \\x~1 z~1 -> ( \\x~2 -> ( ( x~2 y ) z~1 ) ) )"
 ]

main :: IO ()
main = do
	runTestTT ( test [ testMain, testAlpha, testEval ] ) >>= print
	mainGen [ "-e", "main", "./examples/hello.hs" ] [ ] >>= putStr
	src <- readFile "./examples/putStr.hs"
	print $ alphaEnv [ PatVar "x" 0 ] $ alphaEnv [ PatVar "x" 0 ] testEnv
	eval ( load primitives src ) "putStrLn \"Hello, world!\""
