module Tools.Combinators where
import Control.Monad(join)
import Data.IORef (modifyIORef,IORef)

jtraverse :: (Traversable t, Monad t, Monad f) => (a -> f (t b)) -> t a -> f (t b)
jtraverse = fmap join ... traverse

(...) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(...) = (.) . (.)
(....) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(....) = (.) . (...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e

addToIORef :: IORef [a] -> [a] -> IO ()
addToIORef = (. (++)) . modifyIORef