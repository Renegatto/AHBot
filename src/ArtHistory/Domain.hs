module ArtHistory.Domain where
import           ArtHistory.Types
import           Control.Arrow
import           Data.Maybe (maybeToList)
import           Data.List(find)

newQuizSeries :: QuizConfig -> [Event]
newQuizSeries cfg =
    [NewQuizSeriesStarted cfg]

nextQuiz :: QuizConfig -> (Artwork,[Artwork]) -> [Event]
nextQuiz cfg (art,arts) = 
    maybeToList (QuizSended <$> try_quiz)
    where
    variants            = zipWith Variant [0..] arts
    try_quiz            = flip Quiz variants <$> try_right_variant
    try_right_variant   = find ((art ==) . variantArtwork) variants

solveQuiz :: Variant -> Quiz -> [Event]
solveQuiz answer quiz@(Quiz right _)
    |answer == right = 
        [QuizSolved $ Succesful answer quiz]
    |otherwise       = [QuizSolved $ Failed    answer quiz]
trySolve :: Variant -> Quiz -> [Event]
trySolve = undefined



unsolvedQuiz :: [Event] -> Either Error Quiz
unsolvedQuiz = undefined

quizConfig :: [Event] -> Either Error QuizConfig
quizConfig = undefined