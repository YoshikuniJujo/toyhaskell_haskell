{

module NewParser (
 ) where

}

%tokentype	{ Token }

%token
	int	{ TokenInt $$ }

%%

Exp	: int { $1 }
