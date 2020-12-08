module Main where
import qualified Startup as St
import Lib
import Resources (artworks)
main :: IO ()
main = do
    print "starteddd"
    -- storeSample
    print =<< artworks
    print =<< St.startWholeShit
