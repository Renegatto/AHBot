{-# LANGUAGE LambdaCase #-}
module ArtHistory.Commands where

import              Control.Monad                   (liftM2)
import              Tools.Combinators               (jtraverse,(...))

import qualified    ArtHistory.Domain   as    Domain
import              Types.Common        hiding      (Error(..))
import              ArtHistory.Types    hiding      (Result)
import              ArtHistory.Languages.Language
import              Data.List.NonEmpty (NonEmpty)
type ArtworkSet = (Artwork,NonEmpty Artwork)

nextQuiz :: AppL a (Result ())
nextQuiz = do
    cfg <- quizConfig           :: AppL a (Result QuizConfig)
    set <- jtraverse mk_set cfg :: AppL a (Result ArtworkSet)
    jtraverse pushEvents $ liftM2 Domain.nextQuiz cfg set
   where
    mk_set :: QuizConfig -> AppL a (Result ArtworkSet)
    mk_set = randomQuizSet . _cfgTotalVariants

newQuizSeries :: QuizConfig -> AppL a (Result ())
newQuizSeries = pushEvents . Domain.newQuizSeries

solveQuiz :: Answer -> AppL a (Result ())
solveQuiz answer = jtraverse pushEvents 
    . fmap (Domain.solveQuiz answer) 
    =<< unsolvedQuiz

endQuizSeries :: AppL a (Result ())
endQuizSeries = pushEvents . Domain.endQuizSeries 
    =<< subscriptionEvents

handle :: Command -> AppL a (Result ())
handle = \case 
    NextQuiz            -> nextQuiz
    NewQuizSeries cfg   -> newQuizSeries cfg
    SolveQuiz answer    -> solveQuiz answer
    EndQuizSeries       -> endQuizSeries
    SendMessage msg     -> sendMessage msg >> pushEvents [MessageSent msg]
    