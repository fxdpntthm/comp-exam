{-# LANGUAGE MultiParamTypeClasses,  FlexibleInstances #-}
-- {-# , AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies, GADTs, UndecidableInstances, KindSignatures #-}

module TypeClasses where


class Coll e c where
  empty :: c
  insert :: e -> c -> c


instance Coll Int [Int] where
  empty = []
  insert = (:)


instance Coll Char [Char] where
  empty = []
  insert = (:)


t :: [Int]
t = insert (3::Int) (empty::[Int])




-- data Succ n
-- data Zero

-- class Plus x y z | x y -> z
-- instance Plus Zero x x
-- instance Plus x y z => Plus (Succ x) y (Succ z)

-- infixr 5 :::

-- data List :: * -> * -> * where
--    Nil :: List a Zero
--    (:::) :: a -> List a n -> List a (Succ n)

-- append :: Plus x y z => List a x -> List a y -> List a z
-- append Nil ys = ys
-- append (x ::: xs) ys = x ::: append xs ys
