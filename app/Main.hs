module Main where
import qualified Startup as St

main :: IO ()
main = 
    do  -- print =<< sequence (Bot.randomElem [0..10])
        print =<< St.startWholeShit
