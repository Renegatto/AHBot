{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Parsers where
import qualified Constants as Const (bot_prefix,ah_bot_prefix)
import           ArtHistory.Types (Command(..),Art(..),QuizConfig(..),Answer(..))

import           Tools.Combinators((...))
import           Control.Applicative((<|>))
import           Text.Parsec (try,eof,manyTill,Parsec(..),parse,parseTest,optionMaybe,ParseError,unexpected,(<?>),skipMany1,many, ParsecT)
import           Text.Parsec.Char (spaces,anyChar,space,string,digit,char,oneOf,noneOf)
import qualified Data.Text as T
import           Control.Monad(liftM3,void, (>=>))
import           Data.Functor (($>))

import           Data.Either (fromRight)
import           Text.Read(readMaybe)
import qualified Text.Parsec.Token  as P
import Control.Monad.State (MonadState(get), modify, gets)

stringLit :: [Char]
stringLit = ['"',head "'"]

floatSeparators :: [Char]
floatSeparators = [',','.']

not' :: Parser a -> Parser ()
not' p = try $ optionMaybe p >>= maybe (pure ()) (const $ unexpected "")

nat' :: Parser Int
nat'   = read <$> manyTill digit (not' digit)

float' :: Parser Float
float' = read <$> manyTill flo (not' flo)
    where flo = digit <|> oneOf floatSeparators

text' :: Parser String
text' = oneOf stringLit >> manyTill (noneOf stringLit) (oneOf stringLit)

expectedText' :: String -> Parser String
expectedText' e = (oneOf stringLit <?> "string literal")
    >> manyTill (noneOf stringLit) (oneOf stringLit) <?> e

type Parser a = forall s. Parsec String s a

parseAHCommand :: Parser Command
parseAHCommand =
    try parse_new
    <|> try parse_next
    <|> try parse_try
    <|> try parse_done
    where 
    mkNew = (NewQuizSeries .) ... QuizConfig
    command c = string c <?> "command" :: Parser String
    parse_new = command "new" >> do
        variant <- spaces >> (nat' <?> "variants count")
        art     <- spaces >> expectedText' "art name"
        quizes  <- spaces >> (nat' <?> "quizes count")
        pure $ mkNew variant (Art art) quizes
    parse_next = command "next" $> NextQuiz
    parse_try  = command "try"  >> SolveQuiz . Answer <$> (nat' <?> "variant number")
    parse_done = command "done" $> EndQuizSeries
    
parseAH :: String -> Maybe (Either ParseError Command)
parseAH = 
    fmap (parse parseAHCommand "AH command") . parse_prefix
    where
    parse_prefix :: String -> Maybe String
    parse_prefix = 
        either (const Nothing) Just
        . parse (spaces >> string Const.ah_bot_prefix >> many anyChar) "prefix"

{-
cmdLang = P.LanguageDef "" "" "" False anyChar endOfArg space endOfArg ["new","next","try","done"] [] False
tokPars = P.makeTokenParser cmdLang
whiteSpace = P.whiteSpace tokPars
nat   = P.natural tokPars --read <$> manyTill digit endOfArg :: Parsec String u Int
float = P.float tokPars--read <$> manyTill (digit <|> oneOf floatSeparators) endOfArg :: Parsec String u Float
text  = P.stringLiteral tokPars--oneOf stringLit >> manyTill (noneOf stringLit) (oneOf stringLit) :: Parsec String u String
reserved e = P.reserved tokPars--oneOf stringLit >> manyTill (noneOf stringLit) (oneOf stringLit) :: Parsec String u String
-}
