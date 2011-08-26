{

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

}

%name		toyParse
%tokentype	{ Token }

%token
	int	{ TokInteger $$ }

%%

Exp	: int { $1 }

{

happyError = error . show

}
