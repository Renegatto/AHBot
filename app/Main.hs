module Main where
import qualified Startup as St
import Lib
main :: IO ()
main = 
    do  -- print =<< sequence (Bot.randomElem [0..10])
        
        print "started"
        print =<< St.startWholeShit
