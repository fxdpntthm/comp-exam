{-# LANGUAGE NoGADTs #-}

module ADTs where

-- data LamTm a = Var a
--              | Lam (LamTm a)
--              | App (LamTm a) (LamTm b)

-- eval :: LamTm a -> LamTm a
-- eval (App (Lam f) a) = f a
-- eval v               = v


data Alg a  = VAlg a           -- :: Alg a
            -- | IsZero (Alg Int) -- :: Alg a
            | Plus (Alg a) (Alg a) -- :: Alg a

-- data Alg' a where
--   VAlg' :: a -> Alg' a            -- :: Alg a
--             -- | IsZero (Alg Int) -- :: Alg a
--   Plus' :: Alg' a -> Alg' a -> Alg' a -- :: Alg a



zero, one :: Alg Int
zero = VAlg 0
one = VAlg 1

suc :: Alg Int -> Alg Int
suc v = Plus one v

isZero :: Alg Int -> Bool
isZero (VAlg 0) = True
isZero _    = False


eval :: Alg Int -> Int
eval (VAlg i) = i
-- eval (IsZero a) = isZero a
eval (Plus a b) = (eval a) + (eval b)
-- eval (Fun f arg) = f (eval arg)




data AlgExp a  = Value a              -- :: Alg a
               | IsZero (Alg Int)     -- :: Alg a
               | Plus (Alg a) (Alg a) -- :: Alg a

evalAlgExp :: Alg a -> a
evalAlgExp
