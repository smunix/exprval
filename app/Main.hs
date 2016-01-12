module Main where
import Lib

import qualified Data.Map as M
import Control.Monad.Error
import Control.Monad.Reader

{- Abstract : 
   We show how to use monad transformers in order to incrementally add functionality to Haskell programs. This is inspired by Monad Transformers Step by Step article from Martin Grabmuller below,
   http://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf
-}

main4 :: (Monad m) => Env -> ErrorT String m Value
main4 env  = do let e = eval4 $ App (Abs "x" ((Lit 1) `Plus` (Var "x"))) (App (Abs "y" ((Lit 1) `Plus` (Var "y"))) (Var "c"))
                runReaderT e env

withMain4 :: IO (Either String Value)
withMain4 = runErrorT $ main4 (M.fromList [("c", IntV 1)])

main :: IO (Either String Value)
main = withMain4
