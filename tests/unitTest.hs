module Main where

import MainTools
import Test.HUnit
import Alpha
import Parser

testMain :: Test
testMain = test [
	( ~? "eight" ) $ ( == "8" ) `fmap` interpret "./tests/eight.hs" "eight",
	( ~? "ret" ) $ ( == "20" ) `fmap` interpret "./tests/scope1.hs" "ret"
 ]

testAlpha :: Test
testAlpha = test [
	show ( alpha [ ] $ toyParse "\\x y z -> \\x z -> \\x -> x y z" ) ~?=
		"( \\x y z -> ( \\x~1 z~1 -> ( \\x~2 -> ( ( x~2 y ) z~1 ) ) ) )",
	show ( alpha [ ] $ toyParse "let x = 3; y = 2; z = 1; in \\x z -> \\x -> x y z" ) ~?=
		"let z = 1; y = 2; x = 3; in ( \\x~1 z~1 -> ( \\x~2 -> ( ( x~2 y ) z~1 ) ) )"

 ]

main :: IO ()
main = do
	runTestTT testMain >>= print
	runTestTT testAlpha >>= print
	mainGen [ "-e", "main", "./tests/hello.hs" ] [ ]
	print $ alpha [ ] $ toyParse "\\x y z -> \\x z -> \\x -> x y z"
	print $ alpha [ ] $ toyParse "let x = 3; y = 2; z = 1 in \\x z -> \\x -> x y z"
