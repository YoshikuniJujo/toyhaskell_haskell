{

{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-lazy-unlifted-bindings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Parser (
	parse,
	parseModule
 ) where

import Lexer ( Parse, evalParse,
	Token( ReservedId, ReservedOp, Special, TokString, VarSym, Conid, TokChar, Varid, TokEOF,
	TokInteger ), popIndents,
	toyLexer )
import Value ( Value( .. ), Pattern( .. ) )

import "monads-tf" Control.Monad.State ( when, get )

}

%name		parser	Exp
%name		parserModule	Module
%monad		{ Parse }
%lexer		{ toyLexer } { TokEOF }
%tokentype	{ Token }

%token
	varid	{ Varid $$ }
	conid	{ Conid $$ }
	varsym	{ VarSym $$ }
	integer	{ TokInteger $$ }
	char	{ TokChar $$ }
	string	{ TokString $$ }
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
	case	{ ReservedId "case" }
	else	{ ReservedId "else" }
	if	{ ReservedId "if" }
	in	{ ReservedId "in" }
	let	{ ReservedId "let" }
	module	{ ReservedId "module" }
	of	{ ReservedId "of" }
	then	{ ReservedId "then" }
	where	{ ReservedId "where" }
	'_'	{ ReservedId "_" }

%%

Module	: module conid where '{' Decls '}'
				{ Module $5 }

Exp	: LexpOpL Op Exp	{ App ( App ( Var $2 0 ) $1 ) $3 }
	| Lexp			{ $1 }

Op	: varsym		{ $1 }
	| '`' varid '`'		{ $2 }
	| ':'			{ ":" }

Lexp	: '\\' APats '->' Exp	{ Lambda $2 $4 }
	| Let in Exp		{ Letin $1 $3 }
	| if Exp then Exp else Exp
				{ Case $2 [ ( PatCon "True" [ ], $4 ),
						( PatCon "False" [ ], $6 ) ] }
	| LexpOpL		{ $1 }
	| Let			{ Let $1 }

LexpOpL	: case Exp of '{' Alts close
				{ Case $2 $ reverse $5 }
	| Fexp			{ $1 }

Alts	: Alt			{ [ $1 ] }
	| Alts ';'		{ $1 }
	| Alts ';' Alt		{ $3 : $1 }
	| {- empty -}		{ [ ] }

Alt	: Pat '->' Exp		{ ( $1, $3 ) }

Fexp	: Aexp			{ $1 }
	| Fexp Aexp		{ App $1 $2 }

Aexp	: varid			{ Var $1 0 }
	| conid			{ Comp $1 [ ] }
	| integer		{ Integer $1 }
	| char			{ Char $1 }
	| string		{ makeString $1 }
	| '(' ')'		{ Nil }
	| '(' Exp ')'		{ $2 }
	| '[' Elems ']'		{ $2 }

Elems	: {- empty -}		{ Empty }
	| Elems_		{ $1 }

Elems_	: Exp			{ Comp ":" [ $1, Empty ] }
	| Exp ',' Elems_	{ Comp ":" [ $1, $3 ] }

Let	: let '{' Decls close	{ $3 }

Decls	: Decl			{ [ $1 ] }
	| Decls ';'		{ $1 }
	| Decls ';' Decl	{ $3 : $1 }
	| {- empty -}		{ [ ] }

Decl	: Pat '=' Exp		{ ( $1, $3 ) }
	| varid APats '=' Exp	{ ( PatVar $1 0, Lambda $2 $4 ) }

close	: '}'			{ () }
	| error			{% do
					mm <- popIndents
					when ( maybe True ( == 0 ) mm ) $
						happyError }

Pat	: LPat ':' Pat		{ PatCon ":" [ $1, $3 ] }
	| LPat			{ $1 }

LPat	: APat			{ $1 }

APats	: APat			{ [ $1 ] }
	| APat APats		{ $1 : $2 }

APat	: varid			{ PatVar $1 0 }
	| integer		{ PatInteger $1 }
	| '_'			{ PatUScore }
	| '(' Pat ')'		{ $2 }
	| '[' PatLst ']'	{ $2 }

PatLst	: {- empty -}		{ PatEmpty }
	| PatLst_		{ $1 }

PatLst_	: LPat			{ PatCon ":" [ $1, PatEmpty ] }
	| LPat ',' PatLst_	{ PatCon ":" [ $1, $3 ] }

{

parse :: String -> Value
parse input = parser `evalParse` input

parseModule :: String -> Value
parseModule input = parserModule `evalParse` input

makeString :: String -> Value
makeString ""			= Empty
makeString ( '\\' : 'n' : cs )	= Comp ":" [ Char '\n', makeString cs ]
makeString ( '\\' : '\\' : cs )	= Comp ":" [ Char '\\', makeString cs ]
makeString ( c : cs )		= Comp ":" [ Char c, makeString cs ]

happyError :: Parse a
happyError = get >>= error . ( "parse error: " ++ ) . show

}
