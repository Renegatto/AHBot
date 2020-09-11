module ArtHistory.Events where

import ArtHistory.Types
import Types.Common (Subscription(..))
import qualified ArtHistory.Domain as Domain
import qualified Resources as Res (randomQuizSet)

import Discord (DiscordHandler,restCall)
import Discord.Internal.Rest.Channel as RChan
import Discord.Internal.Types (ChannelId)

import Control.Monad.Reader(ReaderT(..))
import qualified Data.Text as T





handleEvent :: Subscription -> Event -> [Command] 
handleEvent sub event =
    case event of
    NewQuizSeriesStarted cfg  -> []
    QuizSended quiz           -> []
    QuizSolved quiz           -> []
    QuizSeriesEnded stats     -> []


{-@

:: Request -> RequestParser -> Command
:: Command -> CommandHandler -> Query [Event] 
-- fetching and collecting data and feeding to domain, returns domain events
:: [Event] -> EventHandler -> Response [Command] 
-- interpreting events, performing actions and generating new commands


@-}