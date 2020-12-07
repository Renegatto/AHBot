module Bot(BotCommand(..),runCommand,createEnv,artHistoryCommand,artHistoryEvent,message2sub) where
import           Data.Functor.Compose
import qualified Data.Text                     as T
import           Control.Monad.Reader          (ReaderT (..), mapReaderT,lift)
import           Control.Monad                 (liftM4,void)
import Data.Either (fromRight)

import           Discord
import           Discord.Internal.Rest
import qualified Discord.Internal.Rest.Channel as RChann
import           Types.Common hiding (Message(..))
-- import           Resources (randomImage)

import           Control.Concurrent.Chan       (newChan,writeChan,writeList2Chan)
import           Data.IORef                    (newIORef,modifyIORef,readIORef)

import qualified ArtHistory.Commands as AHCommands (handle)
import qualified ArtHistory.Languages as AHLangs (AppL,evalAppL,pushEvents) 
import qualified ArtHistory.Types     as AH
import qualified ArtHistory.Events    as EH (handleEvent)
import qualified ArtHistory.Messages  as Messages (Debug(..))
data BotCommand =
  PlsMeme
  |DoNothing

-- hole = undefined

createEnv :: IO (AppData (Sub AH.Event) (Sub AH.Command))
createEnv = liftM4 AppData (newIORef []) newChan (newIORef []) newChan

artHistoryEvent :: AppData (Sub AH.Event) (Sub AH.Command) 
                  -> Sub AH.Event -> DiscordHandler ()
artHistoryEvent appdata event = do
    commands <- sequence <$> (EH.handleEvent event :: DiscordHandler (Sub [AH.Command]))
    lift $ print "commands added"
    lift $ writeList2Chan (commandHub appdata) commands
    lift $ modifyIORef (commandHistory appdata) (commands ++)
    lift $ print "current commandssssssss:"
    lift $ print . map (Messages.Debug . subscriptionStored) 
          =<< readIORef (commandHistory appdata)
  where 
  ss :: Sub [AH.Command] -> [AH.Command]
  ss = subscriptionStored

artHistoryCommand :: AppData (Sub AH.Event) (Sub AH.Command) 
                  -> Sub AH.Command -> DiscordHandler ()
artHistoryCommand appdata (Sub sub command) = do
  lift $ print "evaluatingssss..."
  result <- AHLangs.evalAppL appdata sub (AHCommands.handle command) :: DiscordHandler (Either AH.Error ())
  case result of
    Right x  -> pure $ Right x
    Left err -> AHLangs.evalAppL appdata sub (AHLangs.pushEvents [AH.DomainError err])
  lift $ print "evaluated..."
  pure ()

runCommand :: DiscordHandle -> Message -> BotCommand -> IO ()
runCommand _ _        DoNothing = pure ()
runCommand handle msg PlsMeme = do
    --mimage <- getCompose $ Bot.randomImage
    run $ restCall
        $ RChann.CreateMessage (messageChannel msg) (T.pack "got it :thumbsup:")
    return ()
    where run = flip runReaderT handle

user2sub :: User -> Subscriber
user2sub = Subscriber . userId

message2sub :: Message -> Subscription
message2sub msg = Subscription (user2sub $ messageAuthor msg) (messageChannel msg)

-- ENVIRONMENT