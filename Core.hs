{-# LANGUAGE ExistentialQuantification,
             DeriveDataTypeable,
             OverlappingInstances #-}
module Core where
import Data.Typeable
import Data.Dynamic
import Data.Maybe
import Data.Data

data Showable = forall a. Show a => Showable a
    deriving(Typeable)


retrive = let str = (show $ typeOf retrive)
          in  fromJust $ fromDynamic $ fromJust $ lookup str simpleData

simpleData :: [(String, Dynamic)]
simpleData = zip (map (show.dynTypeRep) sdata') sdata'
  where
    sdata' = [toDyn "5", toDyn $ Showable "4", toDyn "5"]

instance Show Showable where
  show (Showable x) = show x
