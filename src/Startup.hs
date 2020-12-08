module Startup where
import qualified ArtHistory.Bot as Bot
import qualified Types.Common as Common (AppData,Sub(..),_eventsHub,_commandHub,_commandHistory) 
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
import           Data.Functor ((<&>))
import           Data.Maybe (fromMaybe)
import qualified ArtHistory.Events as AH (handleEvent)
import Text.Parsec (ParseError)
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
        ahHandler      Bot.artHistoryCommand   Common._commandHub   app
        >> ahHandler   Bot.artHistoryEvent     Common._eventsHub    app

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
        case event of
        MessageCreate msg ->
            let result = (fromBool (not $ isBotMessage msg) msg
                        >>= parsed)
                        <&> either (parsingFailed msg) 
                                   (lift . command msg)
                            
            in
            fromMaybe (pure ()) result
        _ -> pure ()
    where
    parsingFailed :: Message -> ParseError -> DiscordHandler ()
    parsingFailed msg failmsg =
        lift (print failmsg) >> immediateResponse msg (show failmsg)
    isBotMessage msg =userId (messageAuthor msg) == Const.bot_id
    parsed msg  = Parsers.parseAH (T.unpack . messageText $ msg)
    sub msg     = Common.Sub      (Bot.message2sub msg)
    command :: Message -> AH.Command -> IO ()
    command msg x = do
        print $ "parsed: " <> show x
        writeChan   (Common._commandHub      appdata) (sub msg x  )
        modifyIORef (Common._commandHistory  appdata) (sub msg x :)
fromBool :: Bool -> a -> Maybe a
fromBool True = Just
fromBool False = const Nothing

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

immediateResponse :: Message -> String -> DiscordHandler ()
immediateResponse msg s =
    () <$ handler
    where
    handler = 
        restCall 
        $ RChann.CreateMessage  (messageChannel msg)
                                (T.pack s)
                            