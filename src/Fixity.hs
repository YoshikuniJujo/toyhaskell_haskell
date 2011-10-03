{-# LANGUAGE PatternGuards #-}

module Fixity (fixity, initFix) where

import Prelude hiding (Either(Right, Left))

import Value
import Control.Arrow

data Assoc	= Left | Right | None
data Fixity	= Fix Assoc Int

initFix :: [(String, Fixity)]
initFix = [
	("+", Fix Left 6),
	("-", Fix Left 6),
	("*", Fix Left 7)
 ]

fixity :: [(String, Fixity)] -> Infix -> Value
fixity fl = fixityVal fl . Infix

fixityVal :: [(String, Fixity)] -> Value -> Value
fixityVal fl (Infix i)		= fixityVal fl $ fixityAll fl i
fixityVal fl (Con con mems)	= Con con $ map (fixityVal fl) mems
fixityVal fl (App f a)		= App (fixityVal fl f) $ fixityVal fl a
fixityVal fl (Lambda ps expr)	= Lambda ps $ fixityVal fl expr
fixityVal fl (Case key alts)	= Case (fixityVal fl key) $ map (second $ fixityVal fl) alts
fixityVal fl (Letin defs expr)	= Letin (map (second $ fixityVal fl) defs) $ fixityVal fl expr
fixityVal fl (Module defs)	= Module $ map (second $ fixityVal fl) defs
fixityVal fl (Let defs)		= Let $ map (second $ fixityVal fl) defs
fixityVal _ v			= v

fixityAll :: [(String, Fixity)] -> Infix -> Value
fixityAll _ (Value v)	= v
fixityAll fl i		= fixityAll fl $ fixityOne fl i

fixityOne :: [(String, Fixity)] -> Infix -> Infix
fixityOne fl (Op op1 v1 (Op op2 v2 i))	= case compFixity fl op1 op2 of
	LT	-> Op op1 v1 $ fixityOne fl $ Op op2 v2 i
	_	-> Op op2 (App (App (Var op1 0) (v1 :: Value)) v2) i
fixityOne _ (Op op1 v i)		= Value $ App (App (Var op1 0) v) $ Infix i
fixityOne _ i@(Value _)			= i

compFixity :: [(String, Fixity)] -> String -> String -> Ordering
compFixity fix op1 op2 = case (lookup op1 fix, lookup op2 fix) of
	( Just f1, Just f2 )	-> compInfix f1 f2
	( Just f1, Nothing )	-> compInfix f1 (Fix Left 9)
	( Nothing, Just f2 )	-> compInfix (Fix Left 9) f2
	( Nothing, Nothing )	-> compInfix (Fix Left 9) (Fix Left 9)

compInfix :: Fixity -> Fixity -> Ordering
compInfix (Fix assc1 prec1) (Fix assc2 prec2)
	| prec1 > prec2				= GT
	| prec1 < prec2				= LT
	| Left <- assc1, Left <- assc2		= GT
	| Right <- assc1, Right <- assc2	= LT
	| otherwise				= error "bad associativity"

