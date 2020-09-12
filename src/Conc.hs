module Conc where
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad

commitInputs :: Chan String -> IO ()
commitInputs ch =
    do
        replicateM_ 3 commit
        print "done commiting"
    where 
    commit = do
        print "commiting..." 
        writeChan ch "some_stored_data"

readInputs :: Chan String -> IO ()
readInputs ch = 
    do
        forever read_from_channel
        print "done reading" 
    where 
    read_from_channel = do
        print "reading..."
        data' <- readChan ch
        print $ "readed " <> data'

main' = 
    do
        channel <- newChan

        forkIO . commitInputs $ channel
        forkIO . readInputs   $ channel

        threadDelay $ ms_μs 2000
        print "main thread is done"
        
    where ms_μs = (* 10 ^ 3)