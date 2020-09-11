module Types.Common where
import Discord.Internal.Types (ChannelId,Snowflake)
import qualified Data.Text as T

newtype Image = Image String deriving (Eq,Show)

newtype Subscriber = Subscriber Snowflake

data Subscription = Subscription
    { subscriber            :: Subscriber
    , subsciptionChannel    :: ChannelId }
newtype Message = Message T.Text