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

import Control.Monad.Reader(ReaderT(..),lift)
import qualified Data.Text as T
import Control.Monad (void)

type EventHandler = Event -> DiscordHandler (Sub [Command])
handleEvent :: Sub Event -> DiscordHandler (Sub [Command])
handleEvent event = 
    lift (print "handling event") >> f 
    where 
    f = do
        case subscriptionStored event of

            ( NewQuizSeriesStarted cfg ) -> pure $ [sendMessage event,NextQuiz] <$ event
            ( QuizSended quiz )       -> 
                lift (print "quiz sended" >> pure ([sendMessage event] <$ event))
            ( QuizSolved quiz )       -> pure $ [sendMessage event] <$ event
            ( QuizSeriesEnded stats ) -> pure $ [sendMessage event] <$ event
            ( DomainError e )         -> pure $ [sendMessage event] <$ event
            ( MessageSent _ )         -> pure $ [] <$ event



{-@
:: Request -> RequestParser -> Command
:: Command -> CommandHandler -> Query [Event] 
-- fetching and collecting data and feeding to domain, returns domain events
:: [Event] -> EventHandler -> Response [Command] 
-- interpreting events, performing actions and generating new commands
@-}

--hole = undefined

sendMessage :: Sub Event -> Command
sendMessage (Sub sub event) = SendMessage . flip Message sub . T.pack . show . MessageContent $ event

{-
eventAction :: Subscription -> Event -> DiscordHandler ()
eventAction (Subscription _ channel) =
    void . restCall
    . RChan.CreateMessage channel
    . T.pack . show . MessageContent
-}