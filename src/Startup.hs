module Startup where
import qualified Constants as Const (token,bot_prefix,guildId,chatchannelId)
import qualified Bot
import qualified Parsers as Parsers

import Discord
import Discord.Handle
import Discord.Internal.Gateway 
import Discord.Internal.Rest
import Discord.Internal.Types.Events

import qualified Discord.Internal.Rest.Channel as RChann
import qualified Discord.Internal.Rest.Prelude as Req

import Control.Monad(void)
import Control.Monad.Reader(ReaderT(..),mapReaderT)
import qualified Data.Text as T



data BotCommand = DoNothing | PlsMeme 

runCommand :: DiscordHandle -> Message -> BotCommand -> IO ()
runCommand _ _ DoNothing = pure ()
runCommand handle msg PlsMeme = do
    --mimage <- getCompose $ Bot.randomImage
    run $ restCall 
        $ RChann.CreateMessage (messageChannel msg) (T.pack "got it :thumbsup:")
    return ()
    where run = flip runReaderT handle

startWholeShit :: IO String
startWholeShit = 
    fmap T.unpack 
    $ runDiscord 
    $ runOptions handleStart handleEvent

handleStart :: ReaderT DiscordHandle IO ()
handleStart =
    mapReaderT void
    $ restCall 
    $ RChann.CreateMessage chatchannelId (T.pack "i'm started!")

handleEvent :: Event -> DiscordHandler ()
handleEvent event = ReaderT handler
    where 
    handler handle =
        case event of
            MessageCreate msg ->
                runCommand handle msg
                $ Parsers.parseCommand
                $ messageText msg
            _ -> pure ()

runOptions :: 
    DiscordHandler () ->
    (Event -> DiscordHandler ()) ->
    RunDiscordOpts
runOptions on_start event_handler = RunDiscordOpts 
    {
        discordToken = token,
        discordOnStart = on_start,
        discordOnEnd = print ">discord shutting down",
        discordOnEvent = event_handler,
        discordOnLog = print . ("loging: " <>) . T.unpack,
        discordForkThreadForEvents = True
    }
