module ArtHistory.Commands where
import ArtHistory.Types hiding (Result)
import Types.Common hiding (Error(..))
import qualified ArtHistory.Domain as Domain
import Control.Monad(join,liftM2)--(void,mapM_,join,(<=<),mplus)

import ArtHistory.Languages

(...) = (.) . (.)
jtraverse :: (Traversable t, Monad t, Monad f) => (a -> f (t b)) -> t a -> f (t b)
jtraverse = fmap join ... traverse

-- hole = undefined

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

type CommandHandler = AppContext IO Command [Event]

handle :: Command -> AppL (Result ())
handle event =
    case event of
        NextQuiz -> nextQuiz
        NewQuizSeries cfg -> newQuizSeries cfg
        SolveQuiz answer -> solveQuiz answer
        EndQuizSeries -> endQuizSeries
        SendMessage msg -> sendMessage msg >> pushEvents [MessageSent msg]
    