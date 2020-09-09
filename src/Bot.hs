module Bot(BotCommand(..),runCommand) where
import           Data.Functor.Compose
import qualified Data.Text                     as T
import           Control.Monad.Reader          (ReaderT (..), mapReaderT)

import           Discord
import           Discord.Internal.Rest
import qualified Discord.Internal.Rest.Channel as RChann
import           Types.Common
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

{-
{-# LANGUAGE FlexibleInstances #-}
import Data.Maybe (fromMaybe)
--import Data.Functor(($>))
import Control.Monad(mzero,mplus,MonadPlus(..),join)
import Control.Applicative(Alternative(..))
import Data.Functor.Contravariant

instance Monad IOMaybe where
  return x = Compose $ pure $ Just x
  (Compose x) >>= f = Compose 
      $ (fromMaybe (pure mzero) 
      . fmap (getCompose . f)) =<< x
instance MonadPlus IOMaybe where
  mzero = Compose $ pure Nothing-}