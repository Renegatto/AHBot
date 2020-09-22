module Types.Common where
import Discord.Internal.Types (ChannelId,Snowflake)
import qualified Data.Text as T
import Control.Concurrent.Chan as Chann
import Control.Monad.Reader as Re
newtype Image = Image String deriving (Eq,Show)

newtype Subscriber = Subscriber Snowflake deriving (Eq,Show)

data Subscription = Subscription
    { subscriber            :: Subscriber
    , subscriptionChannel    :: ChannelId } deriving (Eq,Show)
newtype Message = Message T.Text deriving (Eq,Show)

data Sub a = Sub {subscriptionInfo :: Subscription, subscriptionStored :: a }
instance Functor Sub where
    fmap f (Sub sub a) = Sub sub (f a) 
instance Foldable Sub where
    foldr f acc (Sub sub a) = f a acc
instance Traversable Sub where
    sequenceA x@(Sub sub a) = fmap (Sub sub) a

data AppData event command = AppData
    { eventsHistory     :: Chann.Chan event
    , commandHistory    :: Chann.Chan command
    , commandHub        :: Chann.Chan command }

--type AppContext e c a b = Re.Reader (AppData e c, a) b