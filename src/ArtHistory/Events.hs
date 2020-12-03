module ArtHistory.Events where
import ArtHistory.Messages
import ArtHistory.Types
import Types.Common (Subscription(..)
                    ,Message(..)
                    ,subscriptionChannel
                    ,Sub(..)
                    ,subscriptionStored
                    ,subscriptionInfo)
import qualified ArtHistory.Domain as Domain
import qualified Resources as Res (randomQuizSet)

import Discord (DiscordHandler,restCall)
import Discord.Internal.Rest.Channel as RChan
import Discord.Internal.Types (ChannelId)

import Control.Monad.Reader(ReaderT(..))
import qualified Data.Text as T
import Control.Monad (void)

type EventHandler = Event -> DiscordHandler (Sub [Command])
handleEvent :: Sub Event -> DiscordHandler (Sub [Command])
handleEvent event' =
    case subscriptionStored event' of

    event@( NewQuizSeriesStarted cfg ) -> 
        pure $ [sendMessage event, NextQuiz] <$ event'

    event@( QuizSended quiz )       -> pure $ [sendMessage event] <$ event'

    event@( QuizSolved quiz )       -> pure $ [sendMessage event] <$ event'

    event@( QuizSeriesEnded stats ) -> pure $ [sendMessage event] <$ event'

    MessageSent (Message text) ->
        (zero <$) $ restCall
        $ RChan.CreateMessage 
            (subscriptionChannel $ subscriptionInfo event')
            text
        where zero = [] <$ event' 


{-@
:: Request -> RequestParser -> Command
:: Command -> CommandHandler -> Query [Event] 
-- fetching and collecting data and feeding to domain, returns domain events
:: [Event] -> EventHandler -> Response [Command] 
-- interpreting events, performing actions and generating new commands
@-}

--hole = undefined

sendMessage :: Event -> Command
sendMessage = SendMessage . Message . T.pack . show . MessageContent

{-
eventAction :: Subscription -> Event -> DiscordHandler ()
eventAction (Subscription _ channel) =
    void . restCall
    . RChan.CreateMessage channel
    . T.pack . show . MessageContent
-}