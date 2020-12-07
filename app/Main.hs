module Main where
import qualified Startup as St
import Lib
main :: IO ()
main = do
    print "started"
    print =<< St.startWholeShit
