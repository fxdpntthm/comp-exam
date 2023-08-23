{-# LANGUAGE TypeFamilies #-}

module TypeFamilies where

type family F a
type instance F Int = Bool
type instance F Bool = Bool
