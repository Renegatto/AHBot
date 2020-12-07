module ArtHistory.Commands where

import              Control.Monad                   (liftM2)
import              Tools.Combinators               (jtraverse,(...))

import qualified    ArtHistory.Domain   as    Domain
import              Types.Common        hiding      (Error(..))
import              ArtHistory.Types    hiding      (Result)
import              ArtHistory.Languages

type ArtworkSet = (Artwork,[Artwork])

nextQuiz :: AppL (Result ())
nextQuiz = do
    cfg <- quizConfig                                       :: AppL (Result QuizConfig)
    set <- jtraverse (randomQuizSet . cfgTotalVariants) cfg :: AppL (Result ArtworkSet)
    jtraverse pushEvents $ liftM2 Domain.nextQuiz cfg set

newQuizSeries :: QuizConfig -> AppL (Result ())
newQuizSeries = pushEvents . Domain.newQuizSeries

solveQuiz :: Answer -> AppL (Result ())
solveQuiz answer = jtraverse pushEvents . fmap (Domain.solveQuiz answer) =<< unsolvedQuiz

endQuizSeries :: AppL (Result ())
endQuizSeries = pushEvents . Domain.endQuizSeries =<< subscriptionEvents

handle :: Command -> AppL (Result ())
handle event =
    case event of
        NextQuiz -> nextQuiz
        NewQuizSeries cfg -> newQuizSeries cfg
        SolveQuiz answer -> solveQuiz answer
        EndQuizSeries -> endQuizSeries
        SendMessage msg -> sendMessage msg >> pushEvents [MessageSent msg]
    