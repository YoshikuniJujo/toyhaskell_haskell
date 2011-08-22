module Preprocessor (
	Token( .. ),
--	lex,
	prep
) where

import Data.List
-- import Lexer
import Types
import Text.ParserCombinators.Parsec.Pos
import Prelude hiding ( lex )

prep :: Int -> [ Int ] -> [ ( Token, SourcePos ) ] -> [ ( Token, SourcePos ) ]
prep _ idnts [ ]							=
	replicate ( length idnts ) ( CloseBrace, initialPos "" )
prep _ [ ] ( ( NewLine, sp ) : ts )					=
	prep ( sourceColumn sp ) [ ] ts
prep idnt idnts@( i : is ) ( ( NewLine, _ ) : ts@( ( _, sp ) : _ ) )
	| idnt < i	= ( CloseBrace, initialPos "" ) : prep ( sourceColumn sp ) is ts
	| idnt == i	= ( ReservedOp ";", initialPos "" ) : prep ( sourceColumn sp ) idnts ts
	| otherwise	= prep idnt idnts ts
prep idnt ( _ : idnts ) ( ( CloseBrace, ps1 ) : ts )			=
	( CloseBrace, ps1 ) : prep idnt idnts ts
prep idnt idnts ( ( Reserved "of", ps1 ) : ( OpenBrace, ps2 ) : ts )	=
	( Reserved "of", ps1 ) : ( OpenBrace, ps2 ) : prep idnt ( 0 : idnts ) ts
prep _ idnts ( ( Reserved "of", sp1 ) : ( NewLine, _ ) : ( t, sp3 ) : ts )	=
	( Reserved "of", sp1 ) : ( OpenBrace, sp1 ) :
		prep ( sourceColumn sp3 ) ( sourceColumn sp3 : idnts )
			( ( t, sp3 ) : ts )
prep idnt idnts ( ( Reserved "of", ps1 ) : ( t, ps2 ) : ts )		=
	( Reserved "of", ps1 ) : ( OpenBrace, ps1 ) :
		prep idnt ( sourceColumn ps2 : idnts )
		( ( t, ps2 ) : ts )
prep idnt idnts ( t : ts )						=
	t : prep idnt idnts ts
