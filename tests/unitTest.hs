module Main where

import MainTools ( mainGen )
import Test.HUnit
import Alpha
import Parser

interpret :: String -> String -> IO String
interpret fn expr = mainGen [ "-e", expr, fn ] [ ]

testMain :: [ Test ]
testMain = [
	( ~? "eight" ) $ ( == "8\n" ) `fmap` interpret "./examples/eight.hs" "eight",
	( ~? "ret" ) $ ( == "20\n" ) `fmap` interpret "./examples/scope1.hs" "ret"
 ]

testAlpha :: [ Test ]
testAlpha = [
	show ( alpha [ ] $ toyParse "\\x y z -> \\x z -> \\x -> x y z" ) ~?=
		"( \\x y z -> ( \\x~1 z~1 -> ( \\x~2 -> ( ( x~2 y ) z~1 ) ) ) )",
	show ( alpha [ ] $ toyParse "let x = 3; y = 2; z = 1; in \\x z -> \\x -> x y z" ) ~?=
		"let z = 1; y = 2; x = 3; in ( \\x~1 z~1 -> ( \\x~2 -> ( ( x~2 y ) z~1 ) ) )"

 ]

main :: IO ()
main = do
	runTestTT ( test [ testMain, testAlpha ] ) >>= print
	mainGen [ "-e", "main", "./examples/hello.hs" ] [ ] >>= putStr
