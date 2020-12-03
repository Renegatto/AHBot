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

parseArtHistoryCommand :: T.Text -> Maybe Command
parseArtHistoryCommand t = fmap fst . uncurry parseAHArgs =<< parseAHCommand t

data CommandPrefix = New | Next | Try | Done

parseAHCommand :: Parser T.Text CommandPrefix
parseAHCommand s = 
    match Next "next"
    <|> match New "new"
    <|> match Try "try"
    <|> match Done "done"
    where match = (. flip T.stripPrefix s) . fmap . (,)
(...) = (.) . (.)
(....) = (.) . (...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e  
eof' = '_' <$ eof :: Parsec String s Char
sarg = spaces *> manyTill anyChar (eof' <|> space) :: Parsec String s String
arg = read <$> sarg -- :: Read a => Parsec String s a
-- variants:Int (Art name:String) quizes:Int

parseAHArgs' :: Parsec String s Command
parseAHArgs' =
    (    string "new"  >> liftM3 mkNew arg (Art <$> sarg) arg)
    <|> (string "next" $> NextQuiz)
    <|> (string "try"  >> SolveQuiz . Answer <$> arg)
    <|> (string "done" $> EndQuizSeries)
    where mkNew = (NewQuizSeries .) ... QuizConfig
    

parseAHArgs :: CommandPrefix -> Parser T.Text Command 
parseAHArgs prefix s =
    case prefix of
    New  -> new 
    Next -> Just (NextQuiz,s)
    Try  -> finish $ papply (SolveQuiz . Answer) $ pstart readT args
    Done -> Just (EndQuizSeries,s)
    where 
    args = T.words s

    art :: (d,[T.Text]) -> Maybe ((d,Art),[T.Text])
    art = psecond Art . sarg

    new :: Maybe (Command,T.Text)
    new = (pstart readT args >>= art >>= arg) 
        & papply quizConfig 
        & papply NewQuizSeries 
        & finish

    finish = fmap (Bi.second $ T.intercalate " ")
    quizConfig = uncurry . uncurry $ QuizConfig
    sarg = pzip (Just . T.unpack)
    arg  = pzip readT
    readT = readMaybe . T.unpack

type Parser a b = a -> Maybe (b,a) 

arg' :: Read b => (T.Text -> Maybe b) -> (b -> c,[T.Text]) -> Maybe (c,[T.Text])
arg' g (f,x:xs) = (,xs) . f <$> g x
arg' g _        = Nothing

pstart :: (b -> Maybe a) -> [b] -> Maybe (a,[b])
pstart f (x:xs) = (,xs) <$> f x
pstart _ _ = Nothing

pzip :: Read b => (T.Text -> Maybe b) -> (d,[T.Text]) -> Maybe ((d,b),[T.Text])
pzip f (a,x:xs) = (,xs) . (a,) <$> f x
pzip _ _        = Nothing

(&) = flip ($)

papply :: (a -> b) -> Maybe (a,c) -> Maybe (b,c)
papply = fmap . Bi.first
psecond :: (b -> c) -> Maybe ((a,b),e) -> Maybe ((a,c),e)
psecond = fmap . Bi.first . Bi.second


{-
parseAHArgs :: CommandPrefix -> Parser T.Text Command 
parseAHArgs prefix s =
    case prefix of
    New  -> new 
    Next -> Just (NextQuiz,s)
    Try  -> Bi.bimap SolveQuiz (T.intercalate " ") <$> arg (Answer,args)
    Done -> Just (EndQuizSeries,s)
    where 
    args = T.words s :: [T.Text]

    new = Bi.bimap NewQuizSeries (T.intercalate " ") 
        <$> cfg :: Maybe (Command,T.Text)

    art = sarg (Art . T.unpack,args) :: Maybe (Art,[T.Text]) 
    cfg = art >>= arg . Bi.first (flip QuizConfig) >>= arg :: Maybe (QuizConfig,[T.Text])

    sarg = arg' Just :: (T.Text -> c,[T.Text]) -> Maybe (c,[T.Text])
    arg = arg' (readMaybe . T.unpack) :: Read a => (a -> c,[T.Text]) -> Maybe (c,[T.Text])
-}


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
