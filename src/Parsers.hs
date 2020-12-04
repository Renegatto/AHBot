{-# LANGUAGE OverloadedStrings #-}
module Parsers where
import qualified Constants as Const (bot_prefix)
import Bot(BotCommand(..))
import Control.Applicative
import Text.Parsec (eof,manyTill,Parsec(..),parse,optionMaybe)
import Text.Parsec.Char (spaces,anyChar,space,string)
import qualified Data.Text as T
import Control.Monad(liftM3,void)
import Data.Functor (($>))
import ArtHistory.Types (Command(..),Art(..),QuizConfig(..),Answer(..))
import Data.Either (fromRight)
-- hole = undefined

(...) = (.) . (.)
(....) = (.) . (...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e  
eof' = '_' <$ eof
sarg = spaces *> manyTill anyChar (eof' <|> space)
arg = read <$> sarg

parseAHCommand :: Parsec String s Command
parseAHCommand =
    (    string "new"  >> liftM3 mkNew arg (Art <$> sarg) arg)
    <|> (string "next" $> NextQuiz)
    <|> (string "try"  >> SolveQuiz . Answer <$> arg)
    <|> (string "done" $> EndQuizSeries)
    where mkNew = (NewQuizSeries .) ... QuizConfig
    
parseAH :: Parsec String s Command
parseAH = spaces >> string "ah." >> parseAHCommand 
--             |
-- plsmeme bot V

parsePlsMeme :: T.Text -> Maybe BotCommand
parsePlsMeme = fromRight Nothing . parse parser "Pls meme bot" . T.unpack
    where 
    parser = optionMaybe $ parsePrefix >> parseCommand
    
    parsePrefix = spaces >> string (T.unpack Const.bot_prefix)
    parseCommand = 
        string "pls meme" $> PlsMeme
        <|> string "do nothing" $> DoNothing


