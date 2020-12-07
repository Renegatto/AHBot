module Types.Common where
import Discord.Internal.Types (ChannelId,Snowflake)
import qualified Data.Text as T
import Control.Concurrent.Chan as Chann(Chan)
import Data.IORef(IORef)
import Control.Monad.Reader as Re
import qualified Control.Category as Cat
newtype Image = Image String deriving (Eq,Show)

newtype Subscriber = Subscriber Snowflake deriving (Eq,Show)

data Subscription = Subscription
    { subscriber            :: Subscriber
    , subscriptionChannel    :: ChannelId } deriving (Eq,Show)
data Message = Message {_msg :: T.Text, _to :: Subscription} deriving (Eq,Show)

data Sub a = Sub {subscriptionInfo :: Subscription, subscriptionStored :: a } deriving Eq
instance Functor Sub where
    fmap f (Sub sub a) = Sub sub (f a) 
instance Foldable Sub where
    foldr f acc (Sub sub a) = f a acc
instance Traversable Sub where
    sequenceA x@(Sub sub a) = fmap (Sub sub) a

data AppData event command = AppData
    { eventsHistory     :: IORef [event]
    , eventsHub         :: Chann.Chan event
    , commandHistory    :: IORef [command]
    , commandHub        :: Chann.Chan command }

{-instance Show a => Show (Sub a) where
    show (Sub (Subscription (Subscriber sub) schann) v) =
        "    Sub " <> show sub <> " : (\n    " <> show v <> ");\n"-}
--type AppContext e c a b = Re.Reader (AppData e c, a) b