{

{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}
{-# OPTIONS_GHC -fno-warn-lazy-unlifted-bindings #-}

module Parser (parse, parseModule) where

import Preprocessor (Parse, evalParse, popIndent, prep,
	Token(TokInteger, TokChar, TokString, Special, ReservedOp, ReservedId,
		VarSym, ConSym, VarId, ConId, TokEOF))
import Value (
	Value(Nil, Empty, Integer, Char, Var, Comp, App, Lambda, Case, Letin,
		Module, Let),
	Pattern(PatNil, PatEmpty, PatInteger, PatVar, PatCon, PatUScore))

import "monads-tf" Control.Monad.State (when, get)

}

--------------------------------------------------------------------------------

%name		parser		exp
%name		parserModule	module
%monad		{ Parse }
%lexer		{ prep }	{ TokEOF }
%tokentype	{ Token }

%token
	Integer	{ TokInteger $$ }
	Char	{ TokChar $$ }
	String	{ TokString $$ }
	'('	{ Special '(' }
	')'	{ Special ')' }
	','	{ Special ',' }
	';'	{ Special ';' }
	'['	{ Special '[' }
	']'	{ Special ']' }
	'`'	{ Special '`' }
	'{'	{ Special '{' }
	'}'	{ Special '}' }
	':'	{ ReservedOp ":" }
	'='	{ ReservedOp "=" }
	'\\'	{ ReservedOp "\\" }
	'->'	{ ReservedOp "->" }
	Case	{ ReservedId "case" }
	Else	{ ReservedId "else" }
	If	{ ReservedId "if" }
	In	{ ReservedId "in" }
	Let	{ ReservedId "let" }
	Module	{ ReservedId "module" }
	Of	{ ReservedId "of" }
	Then	{ ReservedId "then" }
	Where	{ ReservedId "where" }
	'_'	{ ReservedId "_" }
	'-'	{ VarSym "-" }
	VarSym	{ VarSym $$ }
	ConSym	{ ConSym $$ }
	VarId	{ VarId $$ }
	ConId	{ ConId $$ }

%%

--------------------------------------------------------------------------------

module	: Module ConId Where decls	{ Module $4 }

exp	: infexp			{ $1 }
	| Let decls			{ Let $2 }

infexp	: lexpL op infexp		{ $2 $1 $3 }
	| '-' infexp			{ App ( App ( Var "-" 0 )
						( Integer 0 ) ) $2 }
	| lexp				{ $1 }

lexp	: '\\' apats '->' exp		{ Lambda $2 $4 }
	| Let decls In exp		{ Letin $2 $4 }
	| If exp Then exp Else exp	{ Case $2 [(PatCon "True" [], $4),
						(PatCon "False" [], $6)] }
	| lexpL				{ $1 }

lexpL	: Case exp Of '{' alts close	{ Case $2 $ reverse $5 }
	| fexp				{ $1 }

fexp	: aexp				{ $1 }
	| fexp aexp			{ App $1 $2 }

aexp	: var				{ $1 }
	| gcon				{ $1 }
	| Integer			{ Integer $1 }
	| Char				{ Char $1 }
	| String			{ makeString $1 }
	| '(' exp ')'			{ $2 }
	| '[' elems ']'			{ $2 }

gcon	: '(' ')'			{ Nil }
	| '[' ']'			{ Empty }
	| ConId				{ Comp $1 [] }

varsym	: '-'				{ "-" }
	| VarSym			{ $1 }

var	: VarId				{ Var $1 0 }
	| '(' varsym ')'		{ Var $2 0 }

varop	: varsym			{ $1 }
	| '`' VarId '`'			{ $2 }

conop	: ConSym			{ $1 }
	| ':'				{ ":" }
	| '`' ConId '`'			{ $2 }

op	: varop				{ \x y -> App (App (Var $1 0) x) y }
	| conop				{ \x y -> Comp $1 [ x, y ] }

elems	: exp				{ Comp ":" [$1, Empty] }
	| exp ',' elems			{ Comp ":" [$1, $3] }

alts	: alt				{ [$1] }
	| alts ';'			{ $1 }
	| alts ';' alt			{ $3 : $1 }
	| {- empty -}			{ [] }

alt	: pat '->' exp			{ ($1, $3) }

pat	: lpat conop pat		{ PatCon $2 [$1, $3] }
	| lpat				{ $1 }

lpat	: apat				{ $1 }
	| '-' Integer			{ PatInteger ( - $2 ) }

apat	: VarId				{ PatVar $1 0 }
	| gconPat			{ $1 }
	| Integer			{ PatInteger $1 }
	| '_'				{ PatUScore }
	| '(' pat ')'			{ $2 }
	| '[' patLst ']'		{ $2 }

gconPat	: '(' ')'			{ PatNil }
	| '[' ']'			{ PatEmpty }
	| ConId				{ PatCon $1 [] }

patLst	: pat				{ PatCon ":" [$1, PatEmpty] }
	| pat ',' patLst		{ PatCon ":" [$1, $3] }

decls	: '{' decls_ close		{ $2 }

decls_	: decl				{ [$1] }
	| decls_ ';'			{ $1 }
	| decls_ ';' decl		{ $3 : $1 }
	| {- empty -}			{ [] }

decl	: pat '=' exp			{ ($1, $3) }
	| VarId apats '=' exp		{ (PatVar $1 0, Lambda $2 $4) }

apats	: apat				{ [$1] }
	| apat apats			{ $1 : $2 }

close	: '}'				{ () }
	| error				{% do
					mm <- popIndent
					when (maybe True (== 0) mm) $
						happyError }

--------------------------------------------------------------------------------

{

parse :: String -> Value
parse = (parser `evalParse`)

parseModule :: String -> Value
parseModule = (parserModule `evalParse`)

makeString :: String -> Value
makeString ""			= Empty
makeString ('\\' : 'n' : cs)	= Comp ":" [Char '\n', makeString cs]
makeString ('\\' : '\\' : cs)	= Comp ":" [Char '\\', makeString cs]
makeString (c : cs)		= Comp ":" [Char c, makeString cs]

happyError :: Parse a
happyError = get >>= error . ("parse error: " ++) . show

}
