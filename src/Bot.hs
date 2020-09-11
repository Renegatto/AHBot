module Bot(BotCommand(..),runCommand) where
import           Data.Functor.Compose
import qualified Data.Text                     as T
import           Control.Monad.Reader          (ReaderT (..), mapReaderT)

import           Discord
import           Discord.Internal.Rest
import qualified Discord.Internal.Rest.Channel as RChann
import           Types.Common hiding (Message(..))
import           Resources (randomImage)

data BotCommand =
  PlsMeme
  |DoNothing

runCommand :: DiscordHandle -> Message -> BotCommand -> IO ()
runCommand _ _        DoNothing = pure ()
runCommand handle msg PlsMeme = do
    --mimage <- getCompose $ Bot.randomImage
    run $ restCall
        $ RChann.CreateMessage (messageChannel msg) (T.pack "got it :thumbsup:")
    return ()
    where run = flip runReaderT handle

-- ENVIRONMENT

