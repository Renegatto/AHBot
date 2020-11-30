module ArtHistory.Commands where
import ArtHistory.Types hiding (Result)
import Types.Common hiding (Error(..))
import qualified ArtHistory.Domain as Domain
import Control.Monad(join,liftM2)--(void,mapM_,join,(<=<),mplus)

import ArtHistory.Languages

(...) = (.) . (.)
jtraverse :: (Traversable t, Monad t, Monad f) => (a -> f (t b)) -> t a -> f (t b)
jtraverse = fmap join ... traverse

hole = undefined

type ArtworkSet = (Artwork,[Artwork])

nextQuiz :: AppL (Result ())
nextQuiz = do
    cfg <- quizConfig                                       :: AppL (Result QuizConfig)
    set <- jtraverse (randomQuizSet . cfgTotalVariants) cfg :: AppL (Result ArtworkSet)
    jtraverse pushEvents $ liftM2 Domain.nextQuiz cfg set

newQuizSeries :: QuizConfig -> AppL (Result ())
newQuizSeries = pushEvents . Domain.newQuizSeries

solveQuiz :: Variant -> AppL (Result ())
solveQuiz variant = jtraverse pushEvents . fmap (Domain.trySolve variant) =<< unsolvedQuiz

endQuizSeries :: AppL (Result ())
endQuizSeries = hole

type CommandHandler = AppContext IO Command [Event]

handle :: Command -> AppL (Result ())
handle event =
    case event of
        NextQuiz -> nextQuiz
        NewQuizSeries cfg -> newQuizSeries cfg
        SolveQuiz variant -> solveQuiz variant
        EndQuizSeries -> endQuizSeries
        SendMessage msg -> pushEvents [MessageSent msg]
    