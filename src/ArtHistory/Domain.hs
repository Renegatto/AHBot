module ArtHistory.Domain where
import           ArtHistory.Types
import           Control.Arrow
import           Data.Maybe (maybeToList)
import           Data.List(find)
--hole' = undefined
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
    |otherwise = 
        [QuizSolved $ Failed    answer quiz]
trySolve :: Answer -> Quiz -> [Event]
trySolve = hole'



unsolvedQuiz :: [Event] -> Either Error Quiz
unsolvedQuiz = hole'

quizConfig :: [Event] -> Either Error QuizConfig
quizConfig = hole'