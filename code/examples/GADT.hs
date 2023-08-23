{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}

module GADT where

-- data LamTm a where
--   Var :: String -> LamTm a
--   Lam :: (LamTm a -> LamTm b) -> LamTm (a -> b)
--   App :: LamTm (a -> b) -> LamTm a -> LamTm b


-- eval :: LamTm a -> LamTm a
-- eval (App (Lam f) a) = f a
-- eval v               = v


data Alg a where
  VAlg   :: a -> Alg a
  IsZero :: Alg Int -> Alg Bool
  Plus   :: Alg Int -> Alg Int -> Alg Int
--             | forall b. Fun (Alg a -> Alg b) (Alg a)
  Equals :: Eq a => Alg a -> Alg a -> Alg Bool

zero, one :: Alg Int
zero = VAlg 0
one = VAlg 1

suc :: Alg Int -> Alg Int
suc v = Plus one v

isZero :: Alg Int -> Bool
isZero (VAlg 0) = True
isZero _    = False



eval :: Alg a -> a
eval (VAlg i) = i
eval (IsZero a) = isZero a
eval (Plus a b) = (eval a) + (eval b)
-- eval (Fun f arg) = f (eval arg)
eval (Equals a b) = eval a == eval b



-- type family F a -- = r | r -> a
-- type instance F Int = Int
-- type instance F [Int] = Char
-- -- type instance F a = a

-- data T a = T (F a)


-- type family FClosed a = r | r -> a where
--   -- FClosed [Int] = Int
--   FClosed a = a


-- data T2 a = T2 (FClosed a)
