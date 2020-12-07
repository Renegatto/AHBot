module Startup where
import qualified ArtHistory.Bot as Bot
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
import           Control.Concurrent.Chan       (readChan,writeChan,Chan)
import           Data.IORef                    (modifyIORef)

import           Control.Monad                 (void,forever)
import           Control.Monad.Reader          (ReaderT (..), mapReaderT,MonadReader(..),MonadTrans(..))
import qualified Data.Text                     as T
import           Data.Either (fromRight)
import qualified ArtHistory.Events as AH (handleEvent)

type MyApp = Common.AppData (Common.Sub AH.Event) (Common.Sub AH.Command)

ahHandler :: (p -> a -> ReaderT DiscordHandle IO ()) -> (p -> Chan a) -> p -> DiscordHandler ()
ahHandler handler hub app = 
    let handle  = mapReaderT (void . forkIO . forever) act
        act     = handler app =<< lift (readChan $ hub app)
    in lift (print "CH stared") >> handle

startWholeShit :: IO String
startWholeShit = discordEventHandler =<< Bot.createEnv
    where
    ah_handlers app = 
        ahHandler      Bot.artHistoryCommand   Common.commandHub   app
        >> ahHandler   Bot.artHistoryEvent     Common.eventsHub    app

    all_handlers app = onStart >> ah_handlers app

    discordEventHandler app = 
        fmap T.unpack
        $ runDiscord
        $ runningOptions (all_handlers app) (handleEvent app)

onStart :: DiscordHandler ()
onStart =
    mapReaderT void
    $ restCall
    $ RChann.CreateMessage Const.chatchannelId (T.pack "i'm started!!!")

handleEvent :: MyApp -> Event -> DiscordHandler ()
handleEvent appdata event = 
    lift $  case event of
            MessageCreate msg ->
                if   not $ isBotMessage msg 
                then either print id (command msg <$> parsed msg)
                else pure ()
            _ -> pure ()
    where
    isBotMessage msg = userId (messageAuthor msg) == Const.bot_id
    parsed msg  = Parsers.parseAH (T.unpack . messageText $ msg)
    sub msg     = Common.Sub      (Bot.message2sub msg)
    command msg x = do
        print $ "parsed: " <> show x
        writeChan   (Common.commandHub      appdata) (sub msg x  )
        modifyIORef (Common.commandHistory  appdata) (sub msg x :)


runningOptions :: DiscordHandler () -> (Event -> DiscordHandler ()) -> RunDiscordOpts
runningOptions on_start event_handler = RunDiscordOpts
    {
        discordToken    = Const.token,
        discordOnStart  = on_start,
        discordOnEnd    = print "> discord shutting down",
        discordOnEvent  = event_handler,
        discordOnLog    = print . ("loging: " <>) . T.unpack,
        discordForkThreadForEvents = True
    }

