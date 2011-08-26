module ParserTools (
	makeString
) where

import Types

makeString :: String -> Value
makeString ""			= Empty
makeString ( '\\' : 'n' : cs )	= Complex ":" [ Char '\n', makeString cs ]
makeString ( '\\' : '\\' : cs )	= Complex ":" [ Char '\\', makeString cs ]
makeString ( c : cs )		= Complex ":" [ Char c, makeString cs ]
