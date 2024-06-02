{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances
           , MultiParamTypeClasses, FunctionalDependencies
           , UndecidableInstances, DataKinds #-}

module FunDepGADT where

data Nat = Zero | Suc Nat

data Vector a (b :: Nat) where
  VNil :: Vector a 'Zero
  VCons :: a -> Vector a b -> Vector a ('Suc b)

class Even (a :: Nat)
instance Even 'Zero
instance Even e => Even ('Suc ('Suc e))

-- Have to model this as a class
class Even b => DoubleUp a b | a -> b where
  doubleUp :: Vector x a -> Vector x b

instance DoubleUp 'Zero 'Zero where
  doubleUp VNil = VNil

instance DoubleUp e f => DoubleUp ('Suc e) ('Suc ('Suc f)) where
  doubleUp (VCons x xs) = VCons x (VCons x (doubleUp xs))

-- -- We want to stick a load of these into a list, so we must wrap them up somehow
data AnyLWL a where
  AnyLWL :: Vector a b -> AnyLWL a

-- Now, we would like to apply doubleUp to each element of the list
duAny :: AnyLWL a -> AnyLWL a
duAny (AnyLWL n@VNil)       = AnyLWL $ doubleUp n
duAny (AnyLWL c@(VCons _ _)) = AnyLWL $ doubleUp c


type ZeroN = 'Zero
type OneN = 'Suc ZeroN
type TwoN = 'Suc OneN

v1 :: Vector Char OneN
v1 = VCons 'x' VNil

v2 :: Vector Char TwoN
v2 = VCons 'y' v1

instance Show a => Show (Vector a b) where
  show VNil = ""
  show (VCons x v) = show x ++ show v
