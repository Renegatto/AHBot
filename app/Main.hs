module Main where
import qualified Conc
import Bot
import qualified Startup as St

main :: IO ()
main = 
    do  Conc.main'
        -- print =<< sequence (Bot.randomElem [0..10])
        print =<< St.startWholeShit
