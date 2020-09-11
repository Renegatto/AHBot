module ArtHistory.Domain where
import           ArtHistory.Types
import           Control.Arrow
import           Data.Maybe (maybeToList)
import           Data.List(find)

nextQuiz :: QuizConfig -> (Artwork,[Artwork]) -> [Event]
nextQuiz cfg (art,arts) = 
    NewQuizSeriesStarted cfg : maybeToList (QuizSended <$> try_quiz)
    where
    variants            = zipWith Variant [0..] arts
    try_quiz            = flip Quiz variants <$> try_right_variant
    try_right_variant   = find ((art ==) . variantArtwork) variants

