{-# LANGUAGE TemplateHaskell #-}
module Types.Common where
import qualified Discord.Internal.Types  as Discord(ChannelId,Snowflake)
import qualified Data.Text               as T
import qualified Control.Concurrent.Chan as Chann(Chan)
import qualified Data.IORef              as IORef (IORef)
import           Control.Lens

newtype Image       = Image      String             deriving (Eq,Show)
newtype Subscriber  = Subscriber Discord.Snowflake  deriving (Eq,Show)

data Subscription = Subscription
    { subscriber            :: Subscriber
    , subscriptionChannel   :: Discord.ChannelId } deriving (Eq,Show)
makeLenses ''Subscription
data Message = Message {_msg :: T.Text, _to :: Subscription} deriving (Eq,Show)
makeLenses ''Message
data Sub a = Sub {_subscriptionInfo :: Subscription, _subscriptionStored :: a } deriving Eq
makeLenses ''Sub
data AppData event command = AppData
    { _eventsHistory     :: IORef.IORef [event]
    , _eventsHub         :: Chann.Chan event
    , _commandHistory    :: IORef.IORef [command]
    , _commandHub        :: Chann.Chan command }
makeLenses ''AppData

instance Functor Sub where
    fmap f (Sub sub a) = Sub sub (f a) 
instance Foldable Sub where
    foldr f acc (Sub sub a) = f a acc
instance Traversable Sub where
    sequenceA (Sub sub a) = fmap (Sub sub) a