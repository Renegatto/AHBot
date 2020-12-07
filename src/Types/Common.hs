module Types.Common where
import qualified Discord.Internal.Types  as Discord(ChannelId,Snowflake)
import qualified Data.Text               as T
import qualified Control.Concurrent.Chan as Chann(Chan)
import qualified Data.IORef              as IORef (IORef)


newtype Image       = Image      String             deriving (Eq,Show)
newtype Subscriber  = Subscriber Discord.Snowflake  deriving (Eq,Show)

data Subscription = Subscription
    { subscriber            :: Subscriber
    , subscriptionChannel   :: Discord.ChannelId } deriving (Eq,Show)

data Message = Message {_msg :: T.Text, _to :: Subscription} deriving (Eq,Show)

data Sub a = Sub {subscriptionInfo :: Subscription, subscriptionStored :: a } deriving Eq

data AppData event command = AppData
    { eventsHistory     :: IORef.IORef [event]
    , eventsHub         :: Chann.Chan event
    , commandHistory    :: IORef.IORef [command]
    , commandHub        :: Chann.Chan command }

instance Functor Sub where
    fmap f (Sub sub a) = Sub sub (f a) 
instance Foldable Sub where
    foldr f acc (Sub sub a) = f a acc
instance Traversable Sub where
    sequenceA (Sub sub a) = fmap (Sub sub) a