module Bot(BotCommand(..),runCommand,createEnv,artHistoryCommand) where
import           Data.Functor.Compose
import qualified Data.Text                     as T
import           Control.Monad.Reader          (ReaderT (..), mapReaderT)
import           Control.Monad                 (liftM3,void)
import Data.Either (fromRight)

import           Discord
import           Discord.Internal.Rest
import qualified Discord.Internal.Rest.Channel as RChann
import           Types.Common hiding (Message(..))
import           Resources (randomImage)

import           Control.Concurrent.Chan       (newChan)

import qualified ArtHistory.Commands as AHCommands (handle)
import qualified ArtHistory.Languages as AHLangs (AppL,evalAppL) 
import qualified ArtHistory.Types     as AH

data BotCommand =
  PlsMeme
  |DoNothing

-- hole = undefined

createEnv :: IO (AppData (Sub AH.Event) (Sub AH.Command))
createEnv = liftM3 AppData newChan newChan newChan

artHistoryCommand :: DiscordHandle -> AppData (Sub AH.Event) (Sub AH.Command) -> AH.Command -> IO ()
artHistoryCommand handle appdata command = 
  void $ AHLangs.evalAppL appdata (hole :: Subscription) (fromRight () <$> AHCommands.handle command)

runCommand :: DiscordHandle -> Message -> BotCommand -> IO ()
runCommand _ _        DoNothing = pure ()
runCommand handle msg PlsMeme = do
    --mimage <- getCompose $ Bot.randomImage
    run $ restCall
        $ RChann.CreateMessage (messageChannel msg) (T.pack "got it :thumbsup:")
    return ()
    where run = flip runReaderT handle

-- ENVIRONMENT

