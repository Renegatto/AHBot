{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Parsers where
import qualified Constants as Const (bot_prefix)
import Bot(BotCommand(..))
import Text.Read (readMaybe)
import Control.Applicative
import Control.Arrow(first,second)
import Data.Foldable(fold)
import Data.Monoid(Alt(..))
import Data.Map ((!))
import Data.Maybe(fromMaybe,listToMaybe)
import qualified Data.Map as Map
import Text.Parsec (eof,manyTill,Parsec(..))
import Text.Parsec.Char (spaces,anyChar,space,string)
import qualified Data.Text as T
import Control.Monad(liftM3,void)
import Data.Functor (($>))
import ArtHistory.Types (Command(..),Art(..),QuizConfig(..),Answer(..))
import Data.Bifunctor as Bi (first,bimap,second)

-- hole = undefined

(...) = (.) . (.)
(....) = (.) . (...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e  
eof' = '_' <$ eof
sarg = spaces *> manyTill anyChar (eof' <|> space)
arg = read <$> sarg

parseAHArgs :: Parsec String s Command
parseAHArgs =
    (    string "new"  >> liftM3 mkNew arg (Art <$> sarg) arg)
    <|> (string "next" $> NextQuiz)
    <|> (string "try"  >> SolveQuiz . Answer <$> arg)
    <|> (string "done" $> EndQuizSeries)
    where mkNew = (NewQuizSeries .) ... QuizConfig
    
--             |
-- plsmeme bot V

commands = 
  Map.insert (T.pack "do nothing") DoNothing 
  $ Map.singleton (T.pack "pls meme") PlsMeme 

parseCommand :: T.Text -> BotCommand
parseCommand src =
    fromMaybe DoNothing $ parse_commands =<< parseBotPrefix src
    where parse_commands s = parsePlsMeme s <|> parseDoNothing s

parseDoNothing :: T.Text -> Maybe BotCommand
parseDoNothing = parseCmd (T.pack "do nothing")

parsePlsMeme :: T.Text -> Maybe BotCommand
parsePlsMeme = parseCmd (T.pack "pls meme")

parseCmd :: T.Text -> T.Text -> Maybe BotCommand
parseCmd cmd_name src = 
    (commands ! cmd_name) <$ T.stripPrefix cmd_name (T.strip src)

parseBotPrefix :: T.Text -> Maybe T.Text
parseBotPrefix = T.stripPrefix Const.bot_prefix


{-
{-# LANGUAGE TupleSections #-}
-- ============================ OTHER NOT WORKING SHIT 
newtype Parser a = Parser (T.Text -> (Maybe a,T.Text))
instance Functor Parser where
    fmap f (Parser x) = Parser $ first (fmap f) . x 
instance Applicative Parser where
    pure x = Parser (Nothing,)
    (Parser pab) <*> (Parser pa) = Parser $
        \text -> apply (pab text,pa text)
        where 
            unwrap (Parser p) = p
            apply ((mab,restab),(ma,resta)) = (mab <*> ma, resta)
instance Alternative Parser where
    empty = Parser (Nothing,)
    (Parser x) <|> (Parser y) = undefined

-- data BotCommand = PlsMeme | DoNothing

testStripInfix = do
    print $ stripInfix (T.pack "hello") (T.pack "sdfsdsdhellosdg990")
    print $ stripInfix (T.pack "hello") (T.pack "sdfsdsdsdg990")

stripInfix :: T.Text -> T.Text -> Maybe T.Text
stripInfix to_strip src =
    (T.stripSuffix to_strip =<< flip T.stripSuffix src =<< suffix) <> suffix
    where suffix = getAlt $ foldMap (Alt . T.stripPrefix to_strip) (T.tails src)
-}
-- suffix s x = getAlt $ fold $ map (Alt . T.stripSuffix s) $ T.tails x
