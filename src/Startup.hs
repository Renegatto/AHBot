module Startup where
import qualified Bot
import qualified Types.Common as Common (AppData,Sub(..),eventsHub,commandHub,commandHistory) 
import qualified ArtHistory.Types as AH
import qualified Constants                     as Const ( bot_prefix
                                                        , chatchannelId
                                                        , guildId
                                                        , token
                                                        , bot_id )
import qualified Parsers

import           Discord
import           Discord.Internal.Rest
import qualified Discord.Internal.Rest.Channel as RChann

import           GHC.Conc (forkIO)
import           Control.Concurrent.Chan       (readChan,writeChan)
import           Data.IORef                    (modifyIORef)

import           Control.Monad                 (void,forever)
import           Control.Monad.Reader          (ReaderT (..), mapReaderT,MonadReader(..),MonadTrans(..))
import qualified Data.Text                     as T
import           Data.Either (fromRight)
import qualified ArtHistory.Events as AH (handleEvent)

type MyApp = Common.AppData (Common.Sub AH.Event) (Common.Sub AH.Command)

startWholeShit :: IO String
startWholeShit = discordEventHandler =<< Bot.createEnv
    where
    ah_commandHandler :: MyApp -> DiscordHandler ()
    ah_commandHandler app = 
        let handle = mapReaderT (void . forkIO . forever) $ act
            act = Bot.artHistoryCommand app =<< lift (readChan $ Common.commandHub app) :: DiscordHandler ()
        in lift (print "CH stared") >> handle

    {-ah_eventHandler :: MyApp -> DiscordHandler ()
    ah_eventHandler app = 
        mapReaderT ((print "EH started" >>) . void . forkIO) $ forever 
        $ AH.handleEvent =<< lift (readChan $ Common.eventsHub app)-}
    ah_eventHandler :: MyApp -> DiscordHandler ()
    ah_eventHandler app = 
        let handle = mapReaderT (void . forkIO . forever) $ act
            act = Bot.artHistoryEvent app =<< lift (readChan $ Common.eventsHub app) :: DiscordHandler ()
        in lift (print "EH stared") >> handle 

    ah_handlers app = ah_commandHandler app >> ah_eventHandler   app       
    all_handlers app = handleStart >> ah_handlers app

    discordEventHandler app = 
        fmap T.unpack
        $ runDiscord
        $ runningOptions (all_handlers app) (handleEvent app)

handleStart :: ReaderT DiscordHandle IO ()
handleStart =
    mapReaderT void
    $ restCall
    $ RChann.CreateMessage Const.chatchannelId (T.pack "i'm started!!!")

handleEvent :: MyApp -> Event -> DiscordHandler ()
handleEvent appdata event = ReaderT handler
    where
      handler handle =
        case event of
            MessageCreate msg ->
                if userId (messageAuthor msg) /= Const.bot_id then
                    let parsed  = Parsers.parseAH (T.unpack . messageText $ msg)
                        sub     = Common.Sub (Bot.message2sub msg)
                        command x = do
                            print $ "parsed: " <> show x
                            writeChan   (Common.commandHub      appdata) (sub x  )
                            modifyIORef (Common.commandHistory  appdata) (sub x :)
                    in
                    either print id (command <$> parsed)
                    -- Bot.runCommand handle msg
                    --  $ Parsers.parseCommand
                    --  $ messageText msg
                else pure ()
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

