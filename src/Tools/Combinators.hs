module Tools.Combinators where
import Control.Monad(join)
import Data.IORef (modifyIORef,IORef)

jtraverse :: (Traversable t, Monad t, Monad f) => (a -> f (t b)) -> t a -> f (t b)
jtraverse = fmap join ... traverse

(...) = (.) . (.)
(....) = (.) . (...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e

addToIORef :: IORef [a] -> [a] -> IO ()
addToIORef = (. (++)) . modifyIORef