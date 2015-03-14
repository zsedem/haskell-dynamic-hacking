import System.Plugins.Load(load, LoadStatus(..))
import Core

main :: IO ()
main = do
    module_result <- load "ForDynamiclyLoad.o" ["."] [] "value"
    case module_result of
        LoadFailure msg -> do
            print msg
        LoadSuccess _ (v::Showable) -> do
            print v
