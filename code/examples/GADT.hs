{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module GADT where

data LamTm a where
  Var :: String -> LamTm a
  Lam :: (LamTm a -> LamTm b) -> LamTm (a -> b)
  App :: LamTm (a -> b) -> LamTm a -> LamTm b


eval :: LamTm a -> LamTm a
eval (App (Lam f) a) = f a
