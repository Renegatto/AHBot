module ArtHistory.Events where
import ArtHistory.Messages
import ArtHistory.Types

import Types.Common (Subscription(..)
                    ,Message(..)
                    ,Sub(..)
                    ,subscriptionStored)

import Discord (DiscordHandler)
import qualified Data.Text as T


handleEvent :: Sub Event -> DiscordHandler (Sub [Command])
handleEvent event = do
        case subscriptionStored event of

            ( NewQuizSeriesStarted cfg ) -> pure $ [sendMessage event,NextQuiz] <$ event
            ( QuizSended quiz )       -> pure $ [sendMessage event] <$ event
            ( QuizSolved quiz )       -> pure $ [sendMessage event] <$ event
            ( QuizSeriesEnded stats ) -> pure $ [sendMessage event] <$ event
            ( DomainError e )         -> pure $ [sendMessage event] <$ event
            ( MessageSent _ )         -> pure $ [] <$ event

sendMessage :: Sub Event -> Command
sendMessage (Sub sub event) = SendMessage . flip Message sub . T.pack . show . MessageContent $ event
