module Preprocessor (
	prep
) where

import Data.Char
import Data.List
import Lexer
import Text.ParserCombinators.Parsec.Pos

prep :: Int -> [ Int ] -> [ ( Token, SourcePos ) ] -> [ ( Token, SourcePos ) ]
prep idnt idnts [ ]							=
	replicate ( length idnts ) ( CloseBrace, initPos "" )
prep idnt [ ] ( ( NewLine, sp ) : ts )					=
	prep ( sourceColumn sp ) [ ] ts
prep idnt idnts@( i : is ) ( ( NewLine, ps1 ) : ts@( ( _, sp ) : _ ) )
	| idnt < i	= ( CloseBrace, initPos "" ) : prep ( sourceColumn sp ) is ts
	| idnt == i	= ( ReservedOp ";", initPos "" ) : prep ( sourceColumn sp ) idnts ts
	| otherwise	= prep idnt idnts ts
--	prep ( sourceColumn sp ) idnts ts
prep idnt ( _ : idnts ) ( ( CloseBrace, ps1 ) : ts )			=
	( CloseBrace, ps1 ) : prep idnt idnts ts
prep idnt idnts ( ( Reserved "of", ps1 ) : ( OpenBrace, ps2 ) : ts )	=
	( Reserved "of", ps1 ) : ( OpenBrace, ps2 ) : prep idnt ( 0 : idnts ) ts
prep idnt idnts ( ( Reserved "of", sp1 ) : ( NewLine, sp2 ) : ( t, sp3 ) : ts )	=
	( Reserved "of", sp1 ) : ( OpenBrace, sp1 ) :
		prep ( sourceColumn sp3 ) ( sourceColumn sp3 : idnts )
			( ( t, sp3 ) : ts )
prep idnt idnts ( ( Reserved "of", ps1 ) : ( t, ps2 ) : ts )		=
	( Reserved "of", ps1 ) : ( OpenBrace, ps1 ) :
		prep idnt ( sourceColumn ps2 : idnts )
		( ( t, ps2 ) : ts )
prep idnt idnts ( t : ts )						=
	t : prep idnt idnts ts

addSemi :: Int -> String -> String
addSemi _ "" = ""
addSemi n ( c1 : 'i' : 'n' : c2 : rest )
	| isSpace c1 && isSpace c2 = ' ' : ';' : 'i' : 'n' : ' ' : addSemi n rest
addSemi n0 ( '\n' : rest ) = let n1 = length $ takeWhile ( == '\t' ) rest in
	if n0 >= n1
		then ' ' : ';' : '\n' : addSemi n1 rest
		else '\n' : addSemi n1 rest
addSemi n ( c : cs ) = c : addSemi n cs

eraseImport :: String -> String
eraseImport = unlines . filter ( not . ( "import " `isPrefixOf` ) ) . lines
