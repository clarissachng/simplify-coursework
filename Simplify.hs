module Simplify where

import Expr
import Poly

--------------------------------------------------------------------------------
-- * Task 1
-- Define add, which adds 2 expressions together without introducing
-- any 'junk'.

add :: Expr -> Expr -> Expr
-- add numbers
add (NumLit x) (NumLit y) = NumLit (x + y)

-- when add to 0
add (NumLit 0) expr = expr
add expr (NumLit 0) = expr

-- other cases
add expr1 expr2 = Op AddOp expr1 expr2

--------------------------------------------------------------------------------
-- * Task 2
-- Define mul, which multiplies 2 expressions together without introducing
-- any 'junk'.

mul :: Expr -> Expr -> Expr
-- multiply numbers
mul (NumLit x) (NumLit y) = NumLit (x * y)

-- multiply exponents
mul (ExpX x) (ExpX y) = ExpX (x + y)
mul (ExpX 0) expr     = expr
mul expr     (ExpX 0) = expr

-- when multiply by 0
mul (NumLit 0) expr  = NumLit 0
mul  expr (NumLit 0) = NumLit 0

-- when multiply by 1
mul (NumLit 1) expr  = expr
mul  expr (NumLit 1) = expr

-- other cases
mul expr1 expr2 = Op MulOp expr1 expr2

--------------------------------------------------------------------------------
-- * Task 3
-- Define addAll, which adds a list of expressions together into
-- a single expression without introducing any 'junk'.

addAll :: [Expr] -> Expr
addAll [x] = x
addAll xs  = foldr add (NumLit 0) xs

--------------------------------------------------------------------------------
-- * Task 4
-- Define mulAll, which multiplies a list of expressions together into
-- a single expression without introducing any 'junk'.

mulAll :: [Expr] -> Expr
mulAll [x] = x
mulAll xs  = foldr mul (NumLit 1) xs

--------------------------------------------------------------------------------
-- * Task 5
-- Define exprToPoly, which converts an expression into a polynomial.

exprToPoly :: Expr -> Poly
-- case for numbers & exponents
exprToPoly (NumLit 0) = listToPoly []
exprToPoly (NumLit n) = listToPoly [n]
exprToPoly (ExpX 0)   = listToPoly [1]
exprToPoly (ExpX n)   = listToPoly (1 : replicate n 0)

-- addition operation
exprToPoly (Op AddOp a b) = exprToPoly a + exprToPoly b 

-- multiplication operation
exprToPoly (Op MulOp a b) = exprToPoly a * exprToPoly b

--------------------------------------------------------------------------------
-- * Task 6
-- Define polyToExpr, which converts a polynomial into an expression.

polyToExpr :: Poly -> Expr

-- converts list to expr after polyToList
listToExpr :: [Int] -> Expr
listToExpr []     = NumLit 0
listToExpr [x]    = NumLit x
listToExpr (x:xs) = add (mul (NumLit x) (ExpX (length xs))) (listToExpr xs)

polyToExpr poly = listToExpr (polyToList poly)  

--------------------------------------------------------------------------------
-- * Task 7
-- Define a function which simplifies an expression by converting it to a
-- polynomial and back again.

simplify :: Expr -> Expr
simplify expr = polyToExpr (exprToPoly expr)

--------------------------------------------------------------------------------
