{-# LANGUAGE TypeSynonymInstances #-}

module Main where

data Multi
	= Integer Integer
	| String String
	| IntegerOrString Integer String
	deriving Show

class Eval a where
	getValue :: Multi -> a

instance Eval Integer where
	getValue ( Integer i )			= i
	getValue ( IntegerOrString i _ )	= i

instance Eval String where
	getValue ( String s )			= s
	getValue ( IntegerOrString _ s )	= s

main :: IO ()
main = do
	putStrLn "yet"
	let	some = IntegerOrString 8 "eight"
	print $ ( getValue ( Integer 8 ) :: Integer )
	print $ ( getValue some :: Integer )
	print $ ( getValue some :: String )
