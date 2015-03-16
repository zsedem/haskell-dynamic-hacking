{-# LANGUAGE ExistentialQuantification,
             DeriveDataTypeable,
             KindSignatures,
             PolyKinds,
             OverlappingInstances,
             ScopedTypeVariables,
             ImpredicativeTypes #-}
module Core where
import Data.Typeable
import Data.Dynamic
import Data.Maybe
import Data.Data
import Debug.Trace


retrive simpleData = do
    let nameOfType = (show $ typeOf $ fromJust $ retrive simpleData)
    serviceDyn <- lookup (trace nameOfType nameOfType) simpleData
    service <-  fromDynamic serviceDyn
    return service

simpleData :: [(String, Dynamic)]
simpleData = zip (map (show.dynTypeRep) sdata') sdata'
  where
    sdata' = [toDyn (ExampleService (ExampleServiceImplementation 42))]


-------------example service--------------------
class Show a => ExampleServiceInterface a where
    numberprovider :: a -> Int
data ExampleService = forall a. ExampleServiceInterface a => ExampleService a
    deriving(Typeable)
instance ExampleServiceInterface ExampleService where
    numberprovider (ExampleService a) = numberprovider a
newtype ExampleServiceImplementation = ExampleServiceImplementation { toInt :: Int } deriving (Typeable, Show)
instance ExampleServiceInterface ExampleServiceImplementation where
    numberprovider k = toInt k + 100
-------------example service--------------------


-------------new service--------------------
data NewService = forall a. NewServiceInterface a => NewService a
    deriving(Typeable)
class Show a => NewServiceInterface a where
    constructNewService :: ExampleService -> a
    calculation :: a -> Int -> Int
data NewServiceImplementation = NewServiceImplementation
    { _exampleService :: ExampleService
    } deriving (Typeable, Show)
instance NewServiceInterface NewService where
    constructNewService a = constructNewService a
    calculation (NewService a) b = calculation a b
instance NewServiceInterface NewServiceImplementation where
    constructNewService exampleService =  NewServiceImplementation exampleService
    calculation a b = numberprovider ( _exampleService a) + b
-------------new service--------------------

class Show s => Service s where
    construct :: forall proxy a. Typeable a => proxy a -> s

--------wip-------------------
invoke :: forall a b. (Typeable b, Typeable a) => [(String, Dynamic)] -> b a -> a
invoke serviceMap func = let types = typeRepArgs $ typeOf (func)
                             argTypes = map show $ init types
                             returnType = last types
                             dynFunc = toDyn func
                             resolve nameOfType = lookup nameOfType serviceMap
                             resolvedArgs = map fromJust $ takeWhile isJust $ map resolve argTypes
                         in  fromJust $ fromDynamic $ apply dynFunc resolvedArgs

apply :: Dynamic -> [Dynamic] -> Dynamic
apply f []            = f
apply f (service:xs)  = apply (f `dynApp` service) xs

--------

-----for debugging---------
instance Show ExampleService where
    show (ExampleService a) = "ExampleService " ++ show a

instance Show NewService where
    show (NewService a) = "NewService " ++ show a

