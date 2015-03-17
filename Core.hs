{-# LANGUAGE ExistentialQuantification,
             DeriveDataTypeable,
             KindSignatures,
             PolyKinds,
             OverlappingInstances,
             ScopedTypeVariables,
             ImpredicativeTypes,
             RankNTypes #-}
module Core where
import Data.Typeable
import Data.Dynamic
import Data.Maybe
import Data.Data
import Debug.Trace hiding(trace)


retrive objRegistry = do
    let nameOfType = (show $ typeOf $ fromJust $ retrive objRegistry)
    serviceDyn <- lookup nameOfType objRegistry
    service <-  fromDynamic serviceDyn
    return service

objRegistry :: [(String, Dynamic)]
objRegistry = zip (map (show.dynTypeRep) sdata') sdata'
  where
    exampleService' = ExampleService (ExampleServiceImplementation 42)
    sdata' = [ toDyn exampleService'
             , toDyn (NewService (NewServiceImplementation exampleService'))
             ]


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

-- class Show s => Service s where
--     construct :: forall proxy a. Typeable a => proxy a -> s

--------wip-------------------
invoke :: (Typeable a, Typeable b) => [(String, Dynamic)] -> b -> a
invoke serviceMap func = let types = functionTypeRepArgs (func)
                             argTypes = map show $ trace $ types
                             resolve nameOfType = lookup nameOfType serviceMap
                             resolvedArgs = map fromJust $ take numOfServicesToInvoke $ map resolve argTypes
                             appliedFunc = apply (toDyn func) resolvedArgs
                             result = fromJust $ fromDynamic $ appliedFunc
                             numOfServicesToInvoke = functionArgCount func - functionArgCount result
                         in  result

functionTypeRepArgs :: Typeable a => a -> [TypeRep]
functionTypeRepArgs = functionTypeRepArgs'.typeOf

functionArgCount :: Typeable a => a -> Int
functionArgCount = length.functionTypeRepArgs


functionTypeRepArgs' :: TypeRep -> [TypeRep]
functionTypeRepArgs' typeRep = let  (tyCon, theTypeRepArgs) = splitTyConApp typeRep
                                    (serviceType:restOfTheTypeRep) = theTypeRepArgs
                                    functionTyCon = typeRepTyCon $ typeOf ((\x -> undefined)::Int -> Int)
                               in if tyCon == functionTyCon
                                    then
                                      case restOfTheTypeRep of
                                        []      -> [serviceType]
                                        [x]     -> serviceType:functionTypeRepArgs' x
                                    else
                                      [typeRep]


apply :: Dynamic -> [Dynamic] -> Dynamic
apply f []            = f
apply f (service:xs)  = apply (f `dynApp` service) xs
--------

somefunc :: NewService -> ExampleService -> Int -> Int -> String
somefunc newService exampleService k j =
    "calculation newService k: " ++ show (calculation newService k) ++ "\n" ++
    "numberprovider exampleService + j: " ++ show (numberprovider exampleService + j)
test :: Int -> Int -> String
test = invoke objRegistry somefunc

-----for debugging---------
instance Show ExampleService where
    show (ExampleService a) = "ExampleService " ++ show a

instance Show NewService where
    show (NewService a) = "NewService " ++ show a

trace a = traceShow a a
