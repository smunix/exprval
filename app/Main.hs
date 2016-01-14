module Main where
import Lib

import qualified Data.Map as M
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

{- Abstract : 
   We show how to use monad transformers in order to incrementally add functionality to Haskell programs. This is inspired by Monad Transformers Step by Step article from Martin Grabmuller below,
   http://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf
-}

main4 :: (Monad m) => Env -> ErrorT String m Value
main4 env  = do let e = eval4 $ App (Abs "x" ((Lit 1) `Plus` (Var "x"))) (App (Abs "y" ((Lit 1) `Plus` (Var "y"))) (Var "c"))
                runReaderT e env

withMain4 :: (Monad m) => m (Either String Value)
withMain4 = runErrorT $ main4 (M.fromList [("c", IntV 1)])

withMain5 :: (Monad m) => m (Either String Value, Int)
withMain5 = do let akt = eval5 $ App (Abs "x" ((Lit 1) `Plus` (Var "x"))) (App (Abs "y" ((Lit 1) `Plus` (Var "y"))) (Var "c"))
               let env = M.fromList [("c", IntV 1)]
               runStateT (runErrorT (runReaderT akt env)) 0

withMain6 :: (Monad m) => m ((Either String Value, [String]), Int)
withMain6 = do let akt = eval6 $ App (Abs "x" ((Lit 1) `Plus` (Var "x"))) (App (Abs "y" ((Lit 1) `Plus` (Var "y"))) (Var "c"))
               let env = M.fromList [("c", IntV 1)]
               runStateT (runWriterT (runErrorT (runReaderT akt env))) 0

main :: IO ()
main = do v4 <- withMain4
          print v4
          v5 <- withMain5
          print v5
          v6 <- withMain6
          putStrLn . unlines . snd . fst $ v6
          putStrLn . show . snd $ v6
