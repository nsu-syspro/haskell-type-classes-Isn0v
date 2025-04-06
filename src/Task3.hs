{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- The above pragma enables all warnings

module Task3 where
import Task1 (Parse(..))
import Task2 (Eval(..), Expr(..), evalExpr)

import Data.List (nub)

data BoolOp = And | Or | Xor
  deriving (Show, Eq)

instance Parse BoolOp where
  parse "and" = Just And
  parse "or"  = Just Or
  parse "xor" = Just Xor
  parse _     = Nothing

instance Eval Bool BoolOp where
  evalBinOp And = (&&)
  evalBinOp Or  = (||)
  evalBinOp Xor = (/=)

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
--
solveSAT :: String -> Maybe Bool
solveSAT input = 
  parse input >>= \expr ->
    let vars = nub $ getVariables expr
        combinations = mapM (\v -> [(v, False), (v, True)]) vars
    in Just $ any (isSatisfiable expr) combinations

getVariables :: Expr a op -> [String]
getVariables (Lit _)      = []
getVariables (Var v)      = [v]
getVariables (BinOp _ l r) = getVariables l ++ getVariables r

isSatisfiable :: Expr Bool BoolOp -> [(String, Bool)] -> Bool
isSatisfiable expr assignment = evalExpr assignment expr == Just True

