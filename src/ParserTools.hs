module ParserTools (
	makeString,
	Token( .. ),
	Pattern( .. ),
	Value( .. ),
	ParserMonad,
	popIndents,
	emptyEnv
) where

import Types ( Value( .. ), Token( .. ), Pattern( .. ), ParserMonad,
	popIndents, emptyEnv )

makeString :: String -> Value
makeString ""			= Empty
makeString ( '\\' : 'n' : cs )	= Complex ":" [ Char '\n', makeString cs ]
makeString ( '\\' : '\\' : cs )	= Complex ":" [ Char '\\', makeString cs ]
makeString ( c : cs )		= Complex ":" [ Char c, makeString cs ]
