module Parsers where

import Lib(bot_prefix)
import Startup(BotCommand(..))
import Data.Maybe(fromMaybe)
import Control.Applicative
import Control.Arrow(first,second)
import Data.Foldable(fold)
import Data.Monoid(Alt(..))

import qualified Data.Text as T

parseCommand :: T.Text -> BotCommand
parseCommand src =
    fromMaybe DoNothing $ parse_commands =<< parseBotPrefix src
    where parse_commands s = parsePlsMeme s <|> parseDoNothing s

parsetest = parseCommand (T.pack "pls meme")

parseDoNothing :: T.Text -> Maybe BotCommand
parseDoNothing src =
    DoNothing <$ (T.stripPrefix (T.pack "do nothing") 
    $ T.strip $ src)

parsePlsMeme :: T.Text -> Maybe BotCommand
parsePlsMeme   src = 
    PlsMeme <$ (T.stripPrefix (T.pack "pls meme"  ) 
    $ T.strip $ src)

parseBotPrefix :: T.Text -> Maybe T.Text
parseBotPrefix = T.stripPrefix bot_prefix

-- ============================ OTHER NOT WORKING SHIT
data Parser a = Parser (T.Text -> (Maybe a,T.Text))
instance Functor Parser where
    fmap f (Parser x) = Parser $ first (fmap f) . x 
instance Applicative Parser where
    pure x = Parser (\t -> (Nothing,t))
    (Parser pab) <*> (Parser pa) = Parser $
        \text -> apply (pab text,pa text)
        where 
            unwrap (Parser p) = p
            apply ((mab,restab),(ma,resta)) = (mab <*> ma, resta)
instance Alternative Parser where
    empty = Parser (\x -> (Nothing,x))
    (Parser x) <|> (Parser y) = undefined

-- data BotCommand = PlsMeme | DoNothing

testStripInfix = do
    print $ stripInfix (T.pack "hello") (T.pack "sdfsdsdhellosdg990")
    print $ stripInfix (T.pack "hello") (T.pack "sdfsdsdsdg990")

stripInfix :: T.Text -> T.Text -> Maybe T.Text
stripInfix to_strip src =
    (T.stripSuffix to_strip =<< flip T.stripSuffix src =<< suffix) <> suffix
    where suffix =
                 getAlt $ fold
                 $ map (Alt . T.stripPrefix to_strip)
                 $ T.tails src
-- suffix s x = getAlt $ fold $ map (Alt . T.stripSuffix s) $ T.tails x
