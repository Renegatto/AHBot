module ArtHistory.Domain where
import           Control.Monad ((<=<), (>=>))
import           Data.Maybe (maybeToList,listToMaybe,isJust,isNothing,mapMaybe)
import           Data.List(find)
import           Tools.Combinators ((...))
import           Optics (preview,_head,Swapped (swapped), view, (^?), (%), (%~), Prism')
import           ArtHistory.Types
import           Data.List.NonEmpty as NE(NonEmpty(..),zipWith)
newQuizSeries :: QuizConfig -> [Event]
newQuizSeries cfg = [NewQuizSeriesStarted cfg]

nextQuiz :: QuizConfig -> (Artwork,NonEmpty Artwork) -> [Event]
nextQuiz cfg (art,arts) = 
    maybeToList (QuizSended <$> try_quiz)
    where
    variants            = NE.zipWith Variant (0 :| [1..]) arts
    try_quiz            = flip Quiz variants <$> try_right_variant
    try_right_variant   = find ((art ==) . _variantArtwork) variants

solveQuiz :: Answer -> Quiz -> [Event]
solveQuiz ans@(Answer answer) quiz@(Quiz right variants)
    |answer == _variantNumber right = 
        [QuizSolved $ Succesful right quiz]
    |otherwise =
        case find ((== answer) . _variantNumber) variants of
        Just variant -> [QuizSolved $ Failed (Right variant) quiz]
        Nothing      -> [QuizSolved $ Failed (Left  ans)     quiz]

-- we need some prisms here
unsolvedQuiz :: [Event] -> Either Error Quiz
unsolvedQuiz events = notEnded after >> solved after >> sended before
    where
    (after,before) = span (isNothing . preview _QuizSended) events
    sended   = tryUnpackHead (Error "No quizes has been sent")   _QuizSended 
    notEnded = notHappened   (Error "Quiz series already ended") (isJust . preview _QuizSeriesEnded)
    solved   = notHappened   (Error "Quiz is already solved")    (isJust . preview _QuizSolved     ) 

    unpack (QuizSended quiz) = Just quiz
    unpack _                 = Nothing

endQuizSeries :: [Event] -> [Event]
endQuizSeries = pure . either DomainError QuizSeriesEnded . quizStats

quizStats :: [Event] -> Either Error QuizStats
quizStats events = 
    started before >> notEnded after 
    >> (fmap $ calcQuizStats $ results after) (started before)
    where
    (after,before) = span (isNothing . preview _NewQuizSeriesStarted) events
    results  = mapMaybe      (preview _QuizSolved)
    started  = tryUnpackHead (Error "No quizes has been started") _NewQuizSeriesStarted 
    notEnded = notHappened   (Error "Quiz series already ended") (isJust . preview _QuizSeriesEnded)

calcQuizStats :: [SolvedQuiz] -> QuizConfig -> QuizStats
calcQuizStats results cfg = QuizStats { _statsPassed  = length results
                                      , _statsConfig  = cfg
                                      , _statsSolveds = results}

quizConfig :: [Event] -> Either Error QuizConfig
quizConfig events = notEnded after >> started before
    where
    (after,before) = span (isNothing . preview _NewQuizSeriesStarted) events
    started = tryUnpackHead (Error "No quizes has been started") _NewQuizSeriesStarted 
    notEnded = notHappened (Error "Quiz series already ended") (isJust . preview _QuizSeriesEnded)

tryUnpackHead :: e -> Prism' a b -> [a] -> Either e b
tryUnpackHead e f = maybe (Left e) Right . preview (_head % f)

notHappened :: e -> (a -> Bool) -> [a] -> Either e ()
notHappened e = maybe (Right ()) (const $ Left e) ... find

happened :: e -> (a -> Bool) -> [a] -> Either e a
happened e = maybe (Left e) Right ... find
