module Derive where

import           Syntax

derive :: Expr -> Expr
derive (Var name) = Var ("d" ++ name)
derive (App x y) = App (App (derive x) y) (derive y)
derive (Lam n e) = (Lam n (Lam ("d" ++ n) (derive e)))
derive x = x

-- Lift non-derivative terms to the top of the lambda stack
liftNonDerivativeTerms' :: Expr -> Maybe String -> [Expr] -> [Expr] -> Expr
liftNonDerivativeTerms' x@(Lam n e) ms ts dts =
  case ms of
    Just prevN | ("d" ++ prevN) == n -> liftNonDerivativeTerms' e Nothing ts (dts ++ [x])
    _ -> liftNonDerivativeTerms' e (Just n) (ts ++ [x]) dts
liftNonDerivativeTerms' e _ ts dts = reconstructLambdas ts dts e

-- term lambdas, derivate term lambdas, final expression
reconstructLambdas :: [Expr] -> [Expr] -> Expr -> Expr
reconstructLambdas ((Lam n _):ds) dts e = (Lam n (reconstructLambdas ds dts e))
reconstructLambdas [] ((Lam n _):dts) e = (Lam n (reconstructLambdas [] dts e))
reconstructLambdas [] [] e = e

liftNonDerivativeTerms :: Expr -> Expr
liftNonDerivativeTerms e = liftNonDerivativeTerms' e Nothing [] []
