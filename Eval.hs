module Eval
  ( runEval
  , Scope
  ) where

import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map             as Map
import           Pretty
import           Syntax

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr (Eval.Scope)

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show (VClosure s expr scope) = ("(\\" ++ s ++ " . " ++ ppexpr expr ++ ") -- scope: " ++ show scope)

data EvalState = EvalState
  { depth :: Int
  } deriving (Show)

inc :: Eval a -> Eval a
inc m = do
  modify $ \s -> s { depth = (depth s) + 1 }
  out <- m
  modify $ \s -> s { depth = (depth s) - 1 }
  return out

red :: Expr -> Scope -> Eval ()
red x s = do
  d <- gets depth
  tell [(d, x, s)]
  return ()

type Step = (Int, Expr, Scope)
type Eval a = WriterT [Step] (State EvalState) a

type Scope = Map.Map String Value

eval :: Scope -> Expr -> Eval Value
eval env expr = case expr of

  Lit (LInt x) -> do
    return $ VInt (fromIntegral x)

  Lit (LBool x) -> do
    return $ VBool x

  Var x -> do
    red expr env
    return $ env Map.! x

  Lam x body -> inc $ do
    red body env
    return (VClosure x body env)

  App a b -> inc $ do
    red a env
    x <- eval env a
    red b env
    y <- eval env b
    apply x y

extend :: Scope -> String -> Value -> Scope
extend env v t = Map.insert v t env

apply :: Value -> Value -> Eval Value
apply (VClosure n e clo) ex = do
  eval (extend clo n ex) e
apply _ _  = error "Tried to apply non-closure"

emptyScope :: Scope
emptyScope = Map.empty

runEval :: Expr -> (Value, [Step])
runEval x = evalState (runWriterT (eval emptyScope x)) (EvalState 0)
