module Preprocessor (
	Token( .. ),
	prep
) where

import Prelude hiding ( lex )
import Types ( Token( .. ) )
import Text.ParserCombinators.Parsec.Pos ( SourcePos, initialPos, sourceColumn )

keywords :: [ String ]
keywords = [ "of", "where", "let" ]

sp0 :: SourcePos
sp0 = initialPos ""

ob, cb, semi :: ( Token, SourcePos )
ob	= ( Special '{', sp0 )
cb	= ( Special '}', sp0 )
semi	= ( Special ';', sp0 )

sc :: SourcePos -> Int
sc = sourceColumn

prep :: Int -> [ Int ] -> [ ( Token, SourcePos ) ] -> [ ( Token, SourcePos ) ]
prep _ ia [ ]					= replicate ( length ia ) cb
prep i1 ia [ ( NewLine, _ ) ]			= prep i1 ia [ ]
prep _ [ ] ( ( NewLine, sp ) : ts )		= prep ( sc sp ) [ ] ts
prep i1 ia@( i0 : is ) ( ( NewLine, _ ) : ts@( ( _, sp ) : _ ) )
	| i1 < i0				= cb : prep ( sc sp ) is ts
	| i1 == i0				= semi : prep ( sc sp ) ia ts
	| otherwise				= prep ( sc sp ) ia ts
prep i1 ( _ : is ) ( t@( Special '}', _ ) : ts )	= t : prep i1 is ts
prep i1 ia ( t1@( ReservedId i, _ ) : t2@( Special '{', _ ) : ts )
	| i `elem` keywords			= t1 : t2 : prep i1 ( 0 : ia ) ts
prep _ ia ( t@( ReservedId i, _ ) : ( NewLine, _ ) : ts@( ( _, sp ) : _ ) )
	| i `elem` keywords			= t : ob : prep ( sc sp ) ( sc sp : ia ) ts
prep _ ia ( t@( ReservedId i, _ ) : ts@( ( _, sp ) : _ ) )
	| i `elem` keywords			= t : ob : prep ( sc sp ) ( sc sp : ia ) ts
prep i1 ia ( t : ts )				= t : prep i1 ia ts
