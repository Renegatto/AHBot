{-# LANGUAGE OverloadedStrings #-}
module Parsers where
import qualified Constants as Const (bot_prefix)
import Bot(BotCommand(..))
import Control.Applicative
import Text.Parsec (try,eof,manyTill,Parsec(..),parse,optionMaybe,ParseError,unexpected)
import Text.Parsec.Char (spaces,anyChar,space,string)
import qualified Data.Text as T
import Control.Monad(liftM3,void)
import Data.Functor (($>))
import ArtHistory.Types (Command(..),Art(..),QuizConfig(..),Answer(..))
import Data.Either (fromRight)
import Text.Read(readMaybe)
-- hole = undefined

(...) = (.) . (.)
(....) = (.) . (...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e  
eof' = '_' <$ eof
sarg = spaces *> manyTill anyChar (eof' <|> space)

arg :: Read a => Parsec String n a
arg = maybe (unexpected "failed to read argument :(") pure . readMaybe =<< sarg 

parseAHCommand :: Parsec String s Command
parseAHCommand =
    try parse_new
    <|> try parse_next
    <|> try parse_try
    <|> try parse_done
    where 
    mkNew = (NewQuizSeries .) ... QuizConfig
    parse_new = (string "new"  >> liftM3 mkNew arg (Art <$> sarg) arg)
    parse_next = (string "next" $> NextQuiz)
    parse_try = (string "try"  >> SolveQuiz . Answer <$> arg)
    parse_done = (string "done" $> EndQuizSeries)
    
parseAH :: String -> Either ParseError Command
parseAH = parse (spaces >> string "ah." >> parseAHCommand) ""
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


