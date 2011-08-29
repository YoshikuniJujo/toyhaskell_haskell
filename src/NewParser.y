{

{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-lazy-unlifted-bindings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module NewParser (
	toyParse,
	toyParseModule,
--	toyLex',
--	lexer
 ) where

import Types
import Lexer
import ParserTools
import "monads-tf" Control.Monad.State

}

%name		toyParse	Exp_
%name		toyParseModule	Module
%monad		{ ParserMonad }
%lexer		{ lexer } { TokenEOF }
%tokentype	{ Token }

%token
	int	{ TokInteger $$ }
	string	{ TokString $$ }
	char	{ TokChar $$ }
	varid	{ Varid $$ }
	conid	{ Conid $$ }
	':'	{ ReservedOp ":" }
	'`'	{ Special '`' }
	varsym	{ VarSym $$ }
	let	{ ReservedId "let" }
	in	{ ReservedId "in" }
	'='	{ ReservedOp "=" }
	'{'	{ Special '{' }
	'}'	{ Special '}' }
	'('	{ Special '(' }
	')'	{ Special ')' }
	';'	{ Special ';' }
	'['	{ Special '[' }
	']'	{ Special ']' }
	','	{ Special ',' }
	bslash	{ ReservedOp "\\" }
	'->'	{ ReservedOp "->" }
	if	{ ReservedId "if" }
	then	{ ReservedId "then" }
	else	{ ReservedId "else" }
	case	{ ReservedId "case" }
	of	{ ReservedId "of" }
	module	{ ReservedId "module" }
	where	{ ReservedId "where" }
	'_'	{ ReservedId "_" }

%left in

%%

Exp_	: Exp				{ $1 }
	| Apply Varsym Exp_		{ Apply ( Apply ( Identifier $2 ) $1 ) $3 }
	| Apply ':' Exp_		{ Apply ( Apply ( Identifier ":" ) $1 ) $3 }

Varsym	: varsym			{ $1 }
	| '`' varid '`'			{ $2 }

Exp	: Letin				{ $1 }
	| Let				{ Let $1 }
	| Apply				{ $1 }
	| Lambda			{ $1 }
	| If				{ $1 }
	| Case				{ $1 }

Apply	: Atom				{ $1 }
	| Apply Atom			{ Apply $1 $2 }

Atom	: int				{ Integer $1 }
	| string			{ makeString $1 }
	| char				{ Char $1 }
	| varid				{ Identifier $1 }
	| conid				{ Complex $1 [ ] }
	| Parens			{ $1 }
	| List				{ $1 }

Module	: module conid where '{' Eqs '}'
					{ Let $5 }

Letin	: Let in Exp_			{ Letin $1 $3 }

Let	: let '{' Eqs close		{ $3 }

Eqs	: Eq				{ [ $1 ] }
	| Eqs ';'			{ $1 }
	| Eqs ';' Eq			{ $3 : $1 }
	| {- empty -}			{ [ ] }

Eq	: Pat_ '=' Exp_			{ ( $1, $3 ) }
	| varid Pattern '=' Exp_	{ ( PatVar $1, Lambda emptyEnv [ $2 ] $4 ) }

close	: '}'				{ () }
	| error				{% popIndents >> return () }

Lambda	: bslash Pattern '->' Exp_	{ Lambda emptyEnv [ $2 ] $4 }

Parens	: '(' Exp_ ')'			{ $2 }
	| '(' ')'			{ Nil }

If	: if Exp_ then Exp_ else Exp_	{ Case $2 [ ( PatConst "True" [ ], $4 ),
						( PatConst "False" [ ], $6 ) ] }

Case	: case Exp_ of '{' Cases '}'
					{ Case $2 $ reverse $5 }

Cases	: Case1				{ [ $1 ] }
	| Cases ';'			{ $1 }
	| Cases ';' Case1		{ $3 : $1 }
	| {- empty -}			{ [ ] }

Case1	: Pat_ '->' Exp_		{ ( $1, $3 ) }

List	: '[' Elems_ ']'		{ $2 }

Elems_	: {- empty -}			{ Empty }
	| Elems				{ $1 }

Elems	: Exp_				{ Complex ":" [ $1, Empty ] }
	| Exp_ ',' Elems		{ Complex ":" [ $1, $3 ] }

Pat_	: PatL				{ $1 }

Pattern	: varid				{ PatVar $1 }
	| int				{ PatInteger $1 }
	| '_'				{ PatUScore }
	| '[' PatLst_ ']'		{ $2 }
--	| conid Pats			{ PatConst $1 $2 }
--	| PatL				{ $1 }

-- Pats	: {- empty -}			{ [ ] }
--	| Pattern Pats			{ $1 : $2 }

PatL	: Pattern varsym PatL		{ PatConst $2 [ $1, $3 ] }
	| Pattern ':' PatL		{ PatConst ":" [ $1, $3 ] }
	| Pattern			{ $1 }

PatLst_	: {- empty -}			{ PatEmpty }
	| PatLst			{ $1 }

PatLst	: Pattern			{ PatConst ":" [ $1, PatEmpty ] }
	| Pattern ',' PatLst		{ PatConst ":" [ $1, $3 ] }

{

happyError = get >>= error . ( "parse error: " ++ ) . show

}
