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
	toyLex',
	lexer
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
	varsym	{ VarSym $$ }
	let	{ ReservedId "let" }
	in	{ ReservedId "in" }
	'='	{ ReservedOp "=" }
	'{'	{ Special '{' }
	'}'	{ Special '}' }
	'('	{ Special '(' }
	')'	{ Special ')' }
	';'	{ Special ';' }
	bslash	{ ReservedOp "\\" }
	rarrow	{ ReservedOp "->" }
	if	{ ReservedId "if" }
	then	{ ReservedId "then" }
	else	{ ReservedId "else" }
	module	{ ReservedId "module" }
	where	{ ReservedId "where" }

%left in

%%

Exp_	: Exp				{ $1 }
	| Apply varsym Apply		{ Apply ( Apply ( Identifier $2 ) $1 ) $3 }

Exp	: Letin				{ $1 }
	| Let				{ Let $1 }
	| Apply				{ $1 }
	| Lambda			{ $1 }
	| If				{ $1 }

Apply	: Atom				{ $1 }
	| Apply Atom			{ Apply $1 $2 }

Atom	: int				{ Integer $1 }
	| string			{ makeString $1 }
	| char				{ Char $1 }
	| varid				{ Identifier $1 }
	| conid				{ Complex $1 [ ] }
	| Parens			{ $1 }

Module	: module conid where '{' Eqs '}'
					{ Let $5 }

Letin	: Let in Exp_			{ Letin $1 $3 }

Let	: let '{' Eqs close		{ $3 }

Eqs	: Pattern '=' Exp_		{ [ ( $1, $3 ) ] }
	| Eqs ';'			{ $1 }
	| Eqs ';' Pattern '=' Exp_	{ ( $3, $5 ) : $1 }
	| {- empty -}			{ [ ] }

close	: '}'				{ () }
	| error				{ () }

Lambda	: bslash Pattern rarrow Exp_	{ Lambda emptyEnv [ $2 ] $4 }

Parens	: '(' Exp_ ')'			{ $2 }

If	: if Exp_ then Exp_ else Exp_	{ Case $2 [ ( PatConst "True" [ ], $4 ),
						( PatConst "False" [ ], $6 ) ] }

Pattern	: varid				{ PatVar $1 }

{

happyError = get >>= error . ( "parse error: " ++ ) . show

}
