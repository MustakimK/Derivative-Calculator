module DerivativeCalc where

import Test.QuickCheck

{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -                (abs (2*X + 1)) ^ 3
 -          can be encoded as
 -                Power 3 (Func1 Abs
 -                              (Func2 Add (Func2 Mult (Coef 2) X)
 -                                         (Coef 1)))
 - --------------------------------------------------------------------
 -}
data MathExpr a =
    X
  | Coef a
  | Func1 UnaryOp (MathExpr a)
  | Func2 BinOp (MathExpr a) (MathExpr a)
  deriving (Eq,Show,Read)

data BinOp = Add | Mult
  deriving (Show,Eq,Read)

data UnaryOp = Cos | Sin | Abs | Power Int
  deriving (Show,Eq,Read)

{- -----------------------------------------------------------------
 - eval
 - -----------------------------------------------------------------
 - Description:
 -    Evaluates a expression of type (MathExp a) at v (floating point number)
 -}
eval :: (Floating a, Eq a) => MathExpr a -> a -> a  
eval X v            = v
eval (Coef c) v     = c
eval (Func1 op e) v =
  case op of
    Power n -> a ^^ n
    Cos     -> cos a
    Sin     -> sin a
    Abs     -> abs a
    where
      a = eval e v
eval (Func2 op e0 e1) v =
  case op of
    Add  -> a + b
    Mult -> a * b
    where
      a = eval e0 v
      b = eval e1 v

{- -----------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    Implements the (+), (*), negate, abs and fromInteger methods
 -}
instance Num a => Num (MathExpr a) where
  x + y         = Func2 Add  x y
  x * y         = Func2 Mult x y
  negate x      = Func2 Mult x (Coef (-1))
  abs x         = Func1 Abs  x
  fromInteger i = Coef (fromInteger i)
  signum _      = error "signum is left un-implemented"

{- -----------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    Implements the recip and fromRational methods
 -}
instance Fractional a => Fractional (MathExpr a) where
  recip e        = Func1 (Power (-1)) e
  fromRational e = Coef (fromRational e)

{- -----------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    Implements the pi, sin and cos methods
 -}
instance Floating a => Floating (MathExpr a) where
  pi      = Coef  pi
  sin     = Func1 Sin
  cos     = Func1 Cos
  log     = error "log is left un-implemented"
  asin _  = error "asin is left un-implemented"
  acos _  = error "acos is left un-implemented"
  atan _  = error "atan is left un-implemented"
  sinh _  = error "sinh is left un-implemented"
  cosh _  = error "cosh is left un-implemented"
  tanh _  = error "tanh is left un-implemented"
  asinh _ = error "asinh is left un-implemented"
  acosh _ = error "acosh is left un-implemented"
  atanh _ = error "atanh is left un-implemented"
  exp _   = error "exp is left un-implemented"
  sqrt _  = error "sqrt is left un-implemented"

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description:
 -    Sybolocally differentiates the given MathExpr expression  
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X            = Coef 1
diff (Coef r)     = Coef 0
diff (Func1 op u) = 
  case op of
    Power n   -> Func1 (Power (n-1)) u * Coef(fromIntegral n) * a
    Cos       -> negate (sin u) * a
    Sin       -> cos u * a
    Abs       -> (u / abs u) * a
    where
      a = diff u
diff (Func2 op u v) = 
  case op of
    Add  -> a + b
    Mult -> (a * v) + (u * b)
    where
      a = diff u
      b = diff v

{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description:
 -    Creates a string representation of a given MathExpr
 -}
pretty :: (Show a) => MathExpr a -> String
pretty X             = "X"
pretty (Coef c)      = show c
pretty (Func1 op u0) =
  case op of
    Cos       -> "cos(" ++ a ++ ")"
    Sin       -> "sin(" ++ a ++ ")"
    Abs       -> "abs(" ++ a ++ ")"
    Power d   -> "(" ++ a ++ " ^^ " ++ show d ++ ")"
    where
      a = pretty u0
pretty (Func2 op u0 u1) =
  case op of
    Add  -> "(" ++ pretty u0 ++ " + " ++ pretty u1 ++ ")"
    Mult -> "(" ++ pretty u0 ++ " * " ++ pretty u1 ++ ")"
    where
      a = pretty u0
      b = pretty u1

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}

--------------------------------QuickCheck Tests-------------------------------
infix 4 =~
(=~) :: (Floating a,Ord a) => a -> a -> Bool
x =~ y = abs (x - y) <= 1e-4

{- EXAMPLE
 - Function: eval
 - Property: eval (Func2 Add (Coef x) X) y is correct for all x,y
 - Actual Test Result: Pass
 -}
evalProp0 :: (Float,Float) -> Bool
evalProp0 (x,y) = (x + y) =~ eval (Func2 Add (Coef x) X) y

runEvalProp0 :: IO ()
runEvalProp0 = quickCheck evalProp0

{- EVAL QUICKCHECK 1
 - Function: eval
 - Property: eval (Func2 Mult (Coef x) X) y is correct for all x,y
 - Actual Test Result: Pass
 -}
evalProp1 :: (Float,Float) -> Bool
evalProp1 (x,y) = (x * y) =~ eval (Func2 Mult (Coef x) X) y
 
runEvalProp1 :: IO ()
runEvalProp1 = quickCheck evalProp1

{- DIFF QUICKCHECK 1
 - Function: diff
 - Property: eval (diff (Func1 Sin X)) x is correct for all x
 - Actual Test Result: Pass
 -}
 
diffProp0 :: Float -> Bool
diffProp0 x = cos x =~ eval (diff (Func1 Sin X)) x

runDiffProp0 :: IO ()
runDiffProp0 = quickCheck diffProp0

{-
TEST PLAN
------------------------------------eval----------------------------------------

Function: eval
Test Case Numer: 1
Input: eval (Func2 Add X (Func2 Mult (Coef 2) X)) (2)
Expected Output: 6.0
Actual Output: 6.0

Function: eval
Test Case Numer: 2
Input: eval (Func2 Add X (Func2 Add (Coef 2) X)) (4)
Expected Output: 10.0
Actual Output: 10.0

Function: eval
Test Case Numer: 3
Input: eval (Func2 Mult X (Func2 Mult (Coef 2) X)) (7)
Expected Output: 98.0
Actual Output: 98.0

-----------------------------------diff------------------------------------------

Function: diff
Test Case Numer: 1
Input: diff (Func2 Add X (Func2 Mult (Coef 2) X))
Expected Output: Func2 Add (Coef 1.0) (Func2 Add (Func2 Mult (Coef 0.0) X) (Func2 Mult (Coef 2.0) (Coef 1.0)))
Actual Output: Func2 Add (Coef 1.0) (Func2 Add (Func2 Mult (Coef 0.0) X) (Func2 Mult (Coef 2.0) (Coef 1.0)))

Function: diff
Test Case Numer: 2
Input: diff (Func2 Add X (Func2 Add (Coef 2) X))
Expected Output: Func2 Add (Coef 1.0) (Func2 Add (Coef 0.0) (Coef 1.0))
Actual Output: Func2 Add (Coef 1.0) (Func2 Add (Coef 0.0) (Coef 1.0))

Function: diff
Test Case Numer: 3
Input: diff (Coef 1)
Expected Output: Coef 0.0
Actual Output: Coef 0.0

--------------------------------pretty------------------------------------------

Function: pretty
Test Case Numer: 1
Input: pretty (Func2 Add X (Func2 Mult (Coef 2) X))
Expected Output: "(X + (2 * X))"
Actual Output: "(X + (2 * X))"

Function: pretty
Test Case Numer: 2
Input: pretty (Func2 Add X (Func2 Add (Coef 2) X))
Expected Output: "(X + (2 + X))"
Actual Output: "(X + (2 + X))"

Function: pretty
Test Case Numer: 3
Input: pretty (Func2 Mult X (Func2 Mult (Coef 2) X))
Expected Output: "(X * (2 * X))"
Actual Output: "(X * (2 * X))"

------------------------------------------------------------------------------

END OF TEST PLAN
-}