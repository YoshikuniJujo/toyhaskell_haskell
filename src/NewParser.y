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
	toyLex',
	lexer
 ) where

import Types
import Lexer
import ParserTools
import "monads-tf" Control.Monad.State

}

%name		toyParse
%monad		{ ParserMonad }
%lexer		{ lexer } { TokenEOF }
%tokentype	{ Token }

%token
	int	{ TokInteger $$ }
	string	{ TokString $$ }
	char	{ TokChar $$ }
	varid	{ Varid $$ }
	let	{ ReservedId "let" }
	in	{ ReservedId "in" }
	eq	{ ReservedOp "=" }
	obrc	{ Special '{' }
	cbrc	{ Special '}' }
	opr	{ Special '(' }
	cpr	{ Special ')' }

%left in

%%

Exp	: Letin				{ $1 }
	| Apply				{ $1 }

Apply	: Atom				{ $1 }
	| Apply Atom			{ Apply $1 $2 }

Atom	: int				{ Integer $1 }
	| string			{ makeString $1 }
	| char				{ Char $1 }
	| varid				{ Identifier $1 }
	| Parens			{ $1 }

Letin	: let obrc Pattern eq Exp cbrc in Exp
					{ Letin [ ( $3, $5 ) ] $8 }

Parens	: opr Exp cpr			{ $2 }

Pattern	: varid				{ PatVar $1 }

{

happyError = get >>= error . ( "parse error: " ++ ) . show

}
