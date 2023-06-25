{-# OPTIONS_GHC -fdefer-typed-holes -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List
import qualified Data.Maybe as Maybe

-- | An expression with variables
data Expr a
  = Lit Integer -- ˆ Integer literal
  | Var a -- ˆ Variable
  | Abs (Expr a) -- ˆ Absolute value
  | Sign (Expr a) -- ˆ Signum (1 if positive, 0 if eq zero, -1 if negative)
  | Add (Expr a) (Expr a) -- ˆ Addition
  | Sub (Expr a) (Expr a) -- ˆ Subtraction
  | Mul (Expr a) (Expr a) -- ˆ Multiplication
  | Div (Expr a) (Expr a) -- ˆ Division
  | Greater (Expr a) (Expr a) -- ˆ Greater comparison
  | Less (Expr a) (Expr a) -- ˆ Less comparison
  | Equal (Expr a) (Expr a) -- ˆ Equal comparison
  | GreaterEq (Expr a) (Expr a) -- ˆ Greater or equal comparison
  | LessEq (Expr a) (Expr a) -- ˆ Less or equal comparison
  deriving (Show, Functor)

instance Num (Expr a) where
  e1 + e2 = Add e1 e2
  e1 - e2 = Sub e1 e2
  e1 * e2 = Mul e1 e2
  negate = Sub $ Lit 0
  abs = Abs
  signum = Sign
  fromInteger = Lit

-- | Possible results of `eval` function
data EvalRes
  = IntRes Integer
  | BoolRes Bool

instance Show EvalRes where
  show (IntRes x)  = show x
  show (BoolRes b) = show b

-- | Consideres `eval` result as an integer.
--   In case if it was boolean,
--   produces 1 for True and 0 for False
unwrapAsIntRes :: EvalRes -> Integer
unwrapAsIntRes (IntRes i) = i
unwrapAsIntRes (BoolRes b) =
  if b
    then 1
    else 0

-- | Consideres `eval` result as a boolean.
--   In case if it was integer,
--   produces True for values greater than zero
unwrapAsBoolRes :: EvalRes -> Bool
unwrapAsBoolRes (BoolRes b) = b
unwrapAsBoolRes (IntRes i)  = i > 0

-- | Evaluate an expression with all variables instantiated
eval :: Expr EvalRes -> EvalRes
eval (Lit n) = IntRes n
eval (Var n) = n
eval (Abs n) = IntRes $ abs $ unwrapAsIntRes $ eval n
eval (Sign n) = IntRes $ signum $ unwrapAsIntRes $ eval n
eval (Add e1 e2) = IntRes (unwrapAsIntRes (eval e1) + unwrapAsIntRes (eval e2))
eval (Sub e1 e2) = IntRes (unwrapAsIntRes (eval e1) - unwrapAsIntRes (eval e2))
eval (Mul e1 e2) = IntRes (unwrapAsIntRes (eval e1) * unwrapAsIntRes (eval e2))
eval (Div e1 e2) =
  IntRes (unwrapAsIntRes (eval e1) `div` unwrapAsIntRes (eval e2))
eval (Greater e1 e2) =
  BoolRes (unwrapAsIntRes (eval e1) > unwrapAsIntRes (eval e2))
eval (Less e1 e2) =
  BoolRes (unwrapAsIntRes (eval e1) < unwrapAsIntRes (eval e2))
eval (Equal e1 e2) =
  BoolRes (unwrapAsIntRes (eval e1) == unwrapAsIntRes (eval e2))
eval (GreaterEq e1 e2) =
  BoolRes (unwrapAsIntRes (eval e1) >= unwrapAsIntRes (eval e2))
eval (LessEq e1 e2) =
  BoolRes (unwrapAsIntRes (eval e1) <= unwrapAsIntRes (eval e2))

-- | Display an expression with variables
display :: Expr String -> String
display (Lit n)           = show n
display (Var s)           = s
display (Abs n)           = "|" ++ display n ++ "|"
display (Sign n)          = "Signum(" ++ display n ++ ")"
display (Add e1 e2)       = display e1 ++ " + " ++ display e2
display (Sub e1 e2)       = display e1 ++ " - " ++ display e2
display (Mul e1 e2)       = "(" ++ display e1 ++ ") * (" ++ display e2 ++ ")"
display (Div e1 e2)       = "(" ++ display e1 ++ ") / (" ++ display e2 ++ ")"
display (Greater e1 e2)   = "(" ++ display e1 ++ ") > (" ++ display e2 ++ ")"
display (Less e1 e2)      = "(" ++ display e1 ++ ") < (" ++ display e2 ++ ")"
display (Equal e1 e2)     = "(" ++ display e1 ++ ") == (" ++ display e2 ++ ")"
display (GreaterEq e1 e2) = "(" ++ display e1 ++ ") >= (" ++ display e2 ++ ")"
display (LessEq e1 e2)    = "(" ++ display e1 ++ ") <= (" ++ display e2 ++ ")"

-- | Evaluate an expression with the list of variables
--   In case if variable was not found, produces default value
evalWith ::
     Eq var -- ˆ Variable key (must implement Eq in order to be found)
  => EvalRes -- ˆ Default value in case if variable is not found
  -> [(var, EvalRes)] -- ˆ Associative list of variable keys and values
  -> Expr var -- ˆ Expression to evaluate
  -> EvalRes
evalWith value [] _ = value
evalWith value varList expr = eval $ fmap getValueOrDefault expr
  where
    getValueOrNothing v = snd <$> find (\(key, _) -> key == v) varList
    getValueOrDefault v = Maybe.fromMaybe value $ getValueOrNothing v

-- | Display an expression using a given
--   display function for variables
displayWith :: (var -> String) -> Expr var -> String
displayWith toString expr = display $ fmap toString expr

-- | Substitutes variables in the expression
expandVars ::
     Eq var -- ˆ Variable key (must implement Eq in order to be found)
  => Expr a -- ˆ Default expression to use in case if variable is not found
  -> [(var, a)] -- ˆ Associative list of variable keys and values
  -> Expr var -- ˆ Expression to evaluate
  -> Expr a
expandVars defalt [] _ = defalt
expandVars (Var value) varList expr = fmap getValueOrDefault expr
  where
    getValueOrNothing v = snd <$> find (\(key, _) -> key == v) varList
    getValueOrDefault v = Maybe.fromMaybe value $ getValueOrNothing v
expandVars notVar _ _ = notVar

solution1 :: IO ()
solution1 = do
  let vars = [("x", IntRes 2), ("y", IntRes 3)]
  let x = Var "x"
  let y = Var "y"
  let z = Var "z"
  print $ evalWith (IntRes 0) vars (Add x y)
  print $ evalWith (IntRes 0) vars ((x + y) ^ 2 + z)
  print $ displayWith show (Mul x (Add (Lit 2) y))
  print $ displayWith display (Mul (Var (x + y)) (2 + Var (y ^ 2)))

solution2 :: IO ()
solution2 = do
  let unknown = Var "<unknown>"
  let x = Var "x"
  let y = Var "y"
  let z = Var "z"
  let vars = [("x", "y + z"), ("y", "x + 3")]
  print $ display $ expandVars unknown vars (x * y)
  print $ display $ expandVars unknown vars ((y + z) * (x + 3))
  let uninitialised = Var $ IntRes 0
  let intVars = [("x", IntRes 3), ("y", IntRes 4)]
  print $ eval $ expandVars uninitialised intVars ((y + z) * (x + 3))

main :: IO ()
main = do
  let unknown = Var "<unknown>"
  let x = Var "x"
  let y = Var "y"
  let z = Var "z"
  let vars = [("x", "y + z"), ("y", "x + 3")]
  print $ display $ expandVars unknown vars (Div x y)
  print $ display $ expandVars unknown vars (GreaterEq (y + z) (x + 3))
  let uninitialised = Var $ IntRes 0
  let intVars = [("x", IntRes 3), ("y", IntRes 4)]
  print $ eval $ expandVars uninitialised intVars (Less (y + z) (x + 3))
