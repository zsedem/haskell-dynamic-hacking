import System.Plugins.Load(load_, LoadStatus(..))

main :: IO ()
main = do
    module_result <- load_ "ForDynamiclyLoad.o" [] "value"
    case module_result of
        LoadFailure msg -> do
            print msg
        LoadSuccess _ (v::String) -> do
            print v
