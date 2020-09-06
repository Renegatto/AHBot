--{-# LANGUAGE ScopedTypeVariables #-}
module Conc where
--import Control.Concurrent.STM
--import Control.Concurrent.STM.TChan
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
{-
commitInputs :: TChan String -> IO ()
commitInputs ch =
    do
        replicateM_ 3 commit
        print "done commiting"
    where 
    commit = do
        print "commiting..." 
        atomically $ writeTChan ch "some_stored_data"

readInputs :: TChan String -> IO ()
readInputs ch = 
    do
        forever read_from_channel
        print "done reading" 
    where 
    read_from_channel = do
        print "reading..."
        data' <- atomically $ readTChan ch
        print $ "readed " <> data'

main' = 
    do
        thread1
        threadDelay $ ms_μs 500
        thread2
        threadDelay $ ms_μs 2000
        print "main thread is done"
    where
    chann :: IO (TChan a)
    chann = atomically newTChan

    thread1 :: IO ()
    thread1 = runInTread commitInputs

    thread2 :: IO ()
    thread2 = runInTread readInputs

    runInTread :: (TChan a -> IO ()) -> IO ()
    runInTread f = void . forkIO . f =<< chann

    ms_μs = (* 10 ^ 3)
-}