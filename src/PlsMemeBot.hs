module PlsMemeBot where

import qualified Constants as Const (bot_prefix)

import qualified Data.Text                     as T
import           Discord
import           Discord.Internal.Rest
import qualified Discord.Internal.Rest.Channel as RChann
import           Control.Monad.Reader (runReaderT)
import Control.Applicative
import Text.Parsec (try,eof,manyTill,Parsec(..),parse,optionMaybe,ParseError,unexpected)
import Text.Parsec.Char (spaces,anyChar,space,string)
import Control.Monad(liftM3,void)
import Data.Functor (($>))
import ArtHistory.Types (Command(..),Art(..),QuizConfig(..),Answer(..))
import Data.Either (fromRight)
import Text.Read(readMaybe)

data BotCommand =
  PlsMeme
  |DoNothing
runCommand :: DiscordHandle -> Message -> BotCommand -> IO ()
runCommand _ _        DoNothing = pure ()
runCommand handle msg PlsMeme = do

    run $ restCall
        $ RChann.CreateMessage (messageChannel msg) (T.pack "got it :thumbsup:")
    return ()
    where run = flip runReaderT handle

parsePlsMeme :: T.Text -> Maybe BotCommand
parsePlsMeme = fromRight Nothing . parse parser "Pls meme bot" . T.unpack
    where 
    parser = optionMaybe $ parsePrefix >> parseCommand
    
    parsePrefix = spaces >> string (T.unpack Const.bot_prefix)
    parseCommand = 
        string "pls meme" $> PlsMeme
        <|> string "do nothing" $> DoNothing