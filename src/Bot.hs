{-# LANGUAGE FlexibleInstances #-}
module Bot (randomElem,randomImage) where
import Data.Maybe (fromMaybe)
--import Control.Monad.Trans.Maybe
import Data.Functor.Compose
import Data.Functor(($>))
import Discord
import System.Random (randomRIO)
import Control.Monad(mzero,mplus,MonadPlus(..),join)
import Control.Applicative(Alternative(..))
import Data.Functor.Contravariant
data URI = URI String
data Image = Image URI
newtype SnowflakeID = SnowflakeID Integer

data MessageContent =
  MsgText String
  |MsgImage Image
  |MsgMention SnowflakeID
  |MsgSequence [MessageContent]

data DiscordTarget =
  User SnowflakeID
  |Guild SnowflakeID

data BotCommand =
  SendMeme DiscordTarget
  |DoFlip

data DiscordCommand =
  SendMessage DiscordTarget MessageContent
  |DeleteLastMessage DiscordTarget
  |DoNothing

type IOMaybe = Compose IO Maybe

randomImage :: IOMaybe Image
randomImage = Compose $ sequenceA =<< randomElem <$> images

memeCommand :: BotCommand -> IOMaybe DiscordCommand
memeCommand (SendMeme target) =
  SendMessage target . MsgImage <$> randomImage
memeCommand _ = mzero

doFlipCommand :: BotCommand -> IOMaybe DiscordCommand
doFlipCommand DoFlip = pure  DoNothing
doFlipCommand _      = mzero

processBotCommand :: BotCommand -> IO DiscordCommand
processBotCommand =
  unpack . foldl mplus mzero 
  . flip map [doFlipCommand,memeCommand] . flip ($)
  where 
  unpack x = fromMaybe DoNothing <$> getCompose x

processDiscordCommand :: DiscordCommand -> IO ()
processDiscordCommand cmd = 
  case cmd of
    SendMessage target content  -> sendMessage target content
    DeleteLastMessage target    -> deleteLastMessage target

-- ENVIRONMENT

images :: IO [Image]
images = undefined

sendMessage :: DiscordTarget -> MessageContent -> IO ()
sendMessage = undefined

deleteLastMessage :: DiscordTarget -> IO ()
deleteLastMessage = undefined

randomElem :: [a] -> Maybe (IO a)
randomElem [] = Nothing
randomElem xs = 
  Just $ (xs !!) <$> randomRIO (0,length xs - 1)


instance Monad IOMaybe where
  return x = Compose $ pure $ Just x
  (Compose x) >>= f = Compose 
      $ (fromMaybe (pure mzero) 
      . fmap (getCompose . f)) =<< x
instance MonadPlus IOMaybe where
  mzero = Compose $ pure Nothing