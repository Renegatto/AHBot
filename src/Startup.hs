module Startup where
import qualified Bot
import qualified Types.Common as Common (AppData) 
import qualified ArtHistory.Types as AH
import qualified Constants                     as Const ( bot_prefix
                                                        , chatchannelId
                                                        , guildId
                                                        , token )
import qualified Parsers

import           Discord
import           Discord.Internal.Rest
import qualified Discord.Internal.Rest.Channel as RChann

import           Control.Concurrent.Chan       (getChanContents)
import           Control.Monad                 (void)
import           Control.Monad.Reader          (ReaderT (..), mapReaderT)
import qualified Data.Text                     as T

startWholeShit :: IO String
startWholeShit =
    fmap T.unpack
    . runDiscord
    . runningOptions handleStart . handleEvent 
    =<< Bot.createEnv

handleStart :: ReaderT DiscordHandle IO ()
handleStart =
    mapReaderT void
    $ restCall
    $ RChann.CreateMessage Const.chatchannelId (T.pack "i'm started!")

-- handleEvent :: Common.AppData (Sub AH.Event) (Sub AH.Command) -> Event -> DiscordHandler ()
handleEvent appdata event = ReaderT handler
    where
      handler handle =
        case event of
            MessageCreate msg ->
                Bot.artHistoryCommand handle appdata (Parsers.parseArtHistoryCommand msg)
                -- Bot.runCommand handle msg
                --  $ Parsers.parseCommand
                --  $ messageText msg
            _ -> pure ()

runningOptions :: DiscordHandler () -> (Event -> DiscordHandler ()) -> RunDiscordOpts
runningOptions on_start event_handler = RunDiscordOpts
    {
        discordToken    = Const.token,
        discordOnStart  = on_start,
        discordOnEnd    = print ">discord shutting down",
        discordOnEvent  = event_handler,
        discordOnLog    = print . ("loging: " <>) . T.unpack,
        discordForkThreadForEvents = True
    }

