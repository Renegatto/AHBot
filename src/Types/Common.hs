module Types.Common where
import Discord.Internal.Types (ChannelId,Snowflake)
import qualified Data.Text as T

newtype Image = Image String deriving (Eq,Show)

newtype Subscriber = Subscriber Snowflake deriving (Eq,Show)

data Subscription = Subscription
    { subscriber            :: Subscriber
    , subscriptionChannel    :: ChannelId } deriving (Eq,Show)
newtype Message = Message T.Text deriving (Eq,Show)

data Sub a = Sub {subscriptionStored :: a ,subscriptionInfo :: Subscription}
instance Functor Sub where
    fmap f (Sub a sub) = Sub (f a) sub