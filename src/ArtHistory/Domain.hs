module ArtHistory.Domain where
import           Control.Monad ((<=<))
import           Data.Maybe (maybeToList,listToMaybe,isJust,isNothing,mapMaybe)
import           Data.List(find)
import           Tools.Combinators ((...))
import           Optics (preview)
import           ArtHistory.Types

newQuizSeries :: QuizConfig -> [Event]
newQuizSeries cfg =
    [NewQuizSeriesStarted cfg]

nextQuiz :: QuizConfig -> (Artwork,[Artwork]) -> [Event]
nextQuiz cfg (art,arts) = 
    maybeToList (QuizSended <$> try_quiz)
    where
    variants            = zipWith Variant [0..] arts
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
    sended = tryUnpackHead (preview _QuizSended) (Error "No quizes has been sent")
    notEnded  = notHappened (Error "Quiz series already ended") (isJust . preview _QuizSeriesEnded)
    solved = notHappened (Error "Quiz is already solved")    (isJust . preview _QuizSolved     ) 

    unpack (QuizSended quiz) = Just quiz
    unpack _                 = Nothing

endQuizSeries :: [Event] -> [Event]
endQuizSeries = (:[]) . either DomainError QuizSeriesEnded . quizStats

quizStats :: [Event] -> Either Error QuizStats
quizStats events = 
    started before >> notEnded after 
    >> (fmap $ calcQuizStats $ results after) (started before)
    where
    (after,before) = span (isNothing . preview _NewQuizSeriesStarted) events
    results = mapMaybe (preview _QuizSolved)
    started = tryUnpackHead (preview _NewQuizSeriesStarted) (Error "No quizes has been started")
    notEnded = notHappened (Error "Quiz series already ended") (isJust . preview _QuizSeriesEnded)

calcQuizStats :: [SolvedQuiz] -> QuizConfig -> QuizStats
calcQuizStats results cfg = QuizStats { _statsPassed  = length results
                                      , _statsConfig  = cfg
                                      , _statsSolveds = results}

quizConfig :: [Event] -> Either Error QuizConfig
quizConfig events = notEnded after >> started before
    where
    (after,before) = span (isNothing . preview _NewQuizSeriesStarted) events
    started = tryUnpackHead (preview _NewQuizSeriesStarted) (Error "No quizes has been started")
    notEnded = notHappened (Error "Quiz series already ended") (isJust . preview _QuizSeriesEnded)

tryUnpackHead w e = maybe (Left e) Right . (w <=< listToMaybe)

notE :: Either a e -> Either e a
notE = either Right Left

notHappened :: e -> (a -> Bool) -> [a] -> Either e ()
notHappened e = maybe (Right ()) (const $ Left e) ... find

happened :: e -> (a -> Bool) -> [a] -> Either e a
happened e = maybe (Left e) Right ... find
{-
_newQuizSeriesStarted :: Event -> Maybe QuizConfig
_newQuizSeriesStarted = \case NewQuizSeriesStarted x -> Just x; _ -> Nothing
_quizSeriesEnded :: Event -> Maybe QuizStats
_quizSeriesEnded = \case QuizSeriesEnded x -> Just x; _ -> Nothing
_quizSolved :: Event -> Maybe SolvedQuiz
_quizSolved = \case QuizSolved x -> Just x; _ -> Nothing
_quizSended :: Event -> Maybe Quiz
_quizSended = \case QuizSended x -> Just x; _ -> Nothing
-}