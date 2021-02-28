{-# LANGUAGE DataKinds, TypeApplications #-}
module ArtHistory.Events where
import ArtHistory.Messages ( PolyShow(PShow), ShowFor(ForMessage) )
import ArtHistory.Types
    ( Event(..), Command(SendMessage, NextQuiz) )

import Types.Common ( Message(..)
                    , Sub(..) )

import qualified Data.Text as T
import Data.Functor (($>))

handleEvent :: Sub Event -> Sub [Command]
handleEvent sub@(Sub _ event) =
    sub $>
    case event of
    NewQuizSeriesStarted cfg -> [sendMessage sub,NextQuiz]
    QuizSended quiz          -> [sendMessage sub]
    QuizSolved quiz          -> [sendMessage sub]
    QuizSeriesEnded stats    -> [sendMessage sub]
    DomainError e            -> [sendMessage sub]
    MessageSent _            -> []

sendMessage :: Sub Event -> Command
sendMessage (Sub sub event) = 
     SendMessage . flip Message sub . T.pack 
    . show . PShow @ForMessage $ event
