{-# LANGUAGE ExistentialQuantification #-}
module Core where

data Showable = forall a. Show a => Showable a

instance Show Showable where
  show (Showable x) = show x
