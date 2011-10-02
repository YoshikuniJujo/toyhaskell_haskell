module Fixity (fixity, initFix) where

import Value

data Assoc	= Left | Right | None
data Fixity	= Fix Assoc Int

initFix :: [(String, Fixity)]
initFix = []

fixity :: [(String, Fixity)] -> Infix -> Value
fixity fl = fixityVal fl . Infix
{-
fixity _ (Value v)	= v
fixity fl (Op op x y)	= App (App (Var op 0) $ fixity fl x) $ fixity fl y
fixity fl (Con con vs)	= Con con $ map (fixity fl) vs
fixity _ (Op op x y)	= App (App (Var op 0) x) y
fixity _ v		= v
-}

fixityVal :: [(String, Fixity)] -> Value -> Value
fixityVal fl (Infix i)	= fixityVal fl $ fixityAll fl i
-- fixityVal _ (
fixityVal _ v		= v

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
compFixity _ _ _ = GT
