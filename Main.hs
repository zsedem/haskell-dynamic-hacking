import System.Plugins.Load(pdynload, LoadStatus(..))
import Core

main :: IO ()
main = do
    module_result <- pdynload "ForDynamiclyLoad.o" ["."] [] "Core.Showable" "value"
    case module_result of
        LoadFailure msg -> do
            print msg
        LoadSuccess _ (v::Showable) -> do
            print v
