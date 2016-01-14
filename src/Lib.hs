module Lib
    ( eval6
    , eval5
    , eval4
    , eval3
    , eval2
    , eval1
    , eval0
    , Value (..)
    , Expr (..)
    , Env
    ) where

import qualified Data.Map as M

import Data.Maybe
import Data.Function

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

type Name = String
data Expr = Lit Integer
          | Var Name
          | Plus Expr Expr
          | Abs Name Expr
          | App Expr Expr
          deriving (Show)

type Env = M.Map Name Value

data Value = IntV Integer
           | FunV Env Name Expr
           deriving (Show)

-- 0 : evaluation function
eval0 :: Env -> Expr -> Value
eval0 _ (Lit i) = IntV i
eval0 env (Var x) = fromJust (M.lookup x env)
eval0 env (Plus a b) = on (\(IntV a') (IntV b') -> IntV (a' + b')) (eval0 env) a b
eval0 env (Abs n e) = FunV env n e
eval0 env (App a b) = case (eval0 env a) of
                        FunV env' n' e' -> eval0 (M.insert n' (eval0 env b) env') e'

-- 1 : Monadic call
type Eval1 a = Identity a

-- escape Monad Identity ~ Eval1
-- Eval1 becomes a computation now
runEval1 :: Eval1 a -> a
runEval1 = runIdentity

eval1 :: Env -> Expr -> Eval1 Value
eval1 _ (Lit i) = return $ IntV i
eval1 env (Var x) = return $ fromJust (M.lookup x env)
eval1 env (Plus a b) = (\(IntV x) (IntV y) -> IntV $ x + y) <$> (eval1 env a) <*> (eval1 env b)
eval1 env (Abs n e) = return $ FunV env n e
eval1 env (App a b) = do (FunV env' n' e') <- (eval1 env a)
                         b' <- eval1 env b
                         eval1 (M.insert n' b' env') e'

-- 2 : Generic monadic space
-- Let's change our Monad Eval1 so that the above can work withing any monadic spac
eval2 :: (Monad m) => Env -> Expr -> m Value
eval2 _ (Lit i) = return $ IntV i
eval2 env (Var x) = return $ fromJust (M.lookup x env)
eval2 env (Plus a b) = (\(IntV x) (IntV y) -> IntV $ x + y) <$> (eval2 env a) <*> (eval2 env b)
eval2 env (Abs n e) = return $ FunV env n e
eval2 env (App a b) = do (FunV env' n' a') <- eval2 env a
                         b' <- eval2 env b
                         eval2 (M.insert n' b' env') a'


-- 3 : adding error handling
type Eval3 a = ErrorT String Identity a
eval3 :: Env -> Expr -> Eval3 Value
eval3 env (Var x) = case (M.lookup x env) of
                      Just v -> return $ v
                      _ -> throwError $ show (x) ++ " was not defined!"

eval3 env (Plus a b) = do a' <- eval3 env a
                          b' <- eval3 env b
                          case (a', b') of
                            (IntV x, IntV y) -> return $ IntV (x + y)
                            _ -> throwError $ "Plus : type error! a= " ++ show (a', b')

eval3 env (App a b) = do fa <- eval3 env a
                         vb <- eval3 env b
                         case fa of
                           FunV env' x b' -> eval3 (M.insert x vb env') b'
                           _ -> throwError $ "App : expected a Fun application, but had " ++ show (fa)
eval3 env x = eval2 env x

-- 4 : hiding the environment
-- Welcome the Reader Monad (as a configuration)

type Eval4 m a = ReaderT Env (ErrorT String m) a

eval4 :: (Monad m) => Expr -> Eval4 m Value

eval4 (Lit i) = return $ IntV i

eval4 (Var x) = do env <- ask
                   case (M.lookup x env) of
                     Just v -> return v
                     _ -> throwError $ show (x) ++ " was not defined!"

eval4 (Abs x b) = do env <- ask
                     return $ FunV env x b

eval4 (Plus a b) = do a' <- eval4 a
                      b' <- eval4 b
                      case (a', b') of
                        (IntV x, IntV y) -> return $ IntV $ x + y
                        _ -> throwError $ "Plus : type error! a= " ++ show (a', b')

eval4 (App a b) = do f <- eval4 a
                     x <- eval4 b
                     case f of
                       FunV env' x' b' -> local (const $ M.insert x' x env') $ eval4 b'
                       _ -> throwError $ "App : expected a Fun application, but had " ++ show (f)

-- 5 : Now we have a state that is the number of time eval5 gets ticked.
type Eval5 m a = ReaderT Env (ErrorT String (StateT Int m)) a

tick :: (Num s, MonadState s m) => m ()
tick = get >>= (\x -> put $ x + 1)

eval5 :: (Monad m) => Expr -> Eval5 m Value

eval5 (Lit i) = do tick
                   return $ IntV i

eval5 (Var x) = do tick
                   env <- ask
                   case (M.lookup x env) of
                     Just v -> return v
                     _ -> throwError $ show (x) ++ " was not defined!"

eval5 (Plus a b) = do tick
                      a' <- eval5 a
                      b' <- eval5 b
                      case (a', b') of
                        (IntV x, IntV y) -> return (IntV $ x + y)
                        _ -> throwError $ "Plus : type error! a= " ++ show (a', b')

eval5 (Abs x b) = do tick
                     env <- ask
                     return $ FunV env x b

eval5 (App a b) = do tick
                     f <- eval5 a
                     v <- eval5 b
                     case f of
                       (FunV fe x b) -> local (const (M.insert x v fe)) (eval5 b)
                       _ -> throwError $ "App : expected a Fun application, but had " ++ show (f)

-- 6 : with Writer Monad, to log things
type Eval6 m a = ReaderT Env (ErrorT String (WriterT [String] (StateT Int m))) a

eval6 :: (Monad m) => Expr -> Eval6 m Value

eval6 p@(Lit i) = do tick
                     tell [show p]
                     return $ IntV i

eval6 p@(Var x) = do tick
                     env <- ask
                     case (M.lookup x env) of
                       Just v -> do tell $ [ show p ++ " => " ++ show (v)]
                                    return v
                       _ -> throwError $ show (x) ++ " was not defined!"

eval6 p@(Plus a b) = do tick
                        a' <- eval6 a
                        b' <- eval6 b
                        tell [show p]
                        case (a', b') of
                          (IntV x, IntV y) -> return (IntV $ x + y)
                          _ -> throwError $ "Plus : type error! a= " ++ show (a', b')

eval6 p@(Abs x b) = do tick
                       env <- ask
                       tell [show p]
                       return $ FunV env x b

eval6 p@(App a b) = do tick
                       f <- eval6 a
                       v <- eval6 b
                       tell [show p]
                       case f of
                         (FunV fe x b) -> local (const (M.insert x v fe)) (eval6 b)
                         _ -> throwError $ "App : expected a Fun application, but had " ++ show (f)
