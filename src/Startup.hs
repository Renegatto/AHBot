module Startup where
import qualified Bot
import qualified Constants                     as Const (bot_prefix,
                                                         chatchannelId, guildId,
                                                         token)
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
    $ runDiscord
    $ runningOptions handleStart handleEvent

handleStart :: ReaderT DiscordHandle IO ()
handleStart =
    mapReaderT void
    $ restCall
    $ RChann.CreateMessage Const.chatchannelId (T.pack "i'm started!")

handleEvent :: Event -> DiscordHandler ()
handleEvent event = ReaderT handler
    where
      handler handle =
        case event of
            MessageCreate msg ->
                Bot.runCommand handle msg
                $ Parsers.parseCommand
                $ messageText msg
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

