{-# LANGUAGE LambdaCase #-}
module ArtHistory.Domain where

import           ArtHistory.Types

import           Data.Maybe (maybeToList,listToMaybe,isJust,isNothing)
import           Data.List(find,break)
hole' = 3
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

solveQuiz :: Answer -> Quiz -> [Event]
solveQuiz ans@(Answer answer) quiz@(Quiz right variants)
    |answer == variantNumber right = 
        [QuizSolved $ Succesful right quiz]
    |otherwise =
        case find ((== answer) . variantNumber) variants of
        Just variant -> [QuizSolved $ Failed (Right variant) quiz]
        Nothing      -> [QuizSolved $ Failed (Left  ans)     quiz]

-- we need some prisms here
unsolvedQuiz :: [Event] -> Either Error Quiz
unsolvedQuiz events = ended after >> solved after >> sended
    where
    (after,before) = break (isNothing . _quizSended) events
    sended = maybe (Left $ Error "No quizes has been sent") Right 
                   (_quizSended =<< listToMaybe before)
    ended  = check (Error "Quiz series already ended") (isJust . _quizSeriesEnded)
    solved = check (Error "Quiz is already solved")    (isJust . _quizSolved     ) 

    unpack (QuizSended quiz) = Just quiz
    unpack _                 = Nothing

endQuizSeries :: [Event] -> [Event]
endQuizSeries = 8

quizConfig :: [Event] -> Either Error QuizConfig
quizConfig events = ended after >> started
    where
    (after,before) = break (isNothing . _newQuizSeriesStarted) events
    started = maybe (Left $ Error "No quizes has been started") Right 
                    (_newQuizSeriesStarted =<< listToMaybe before)
    ended = check (Error "Quiz series already ended") (isJust . _quizSeriesEnded)


check :: e -> (a -> Bool) -> [a] -> Either e ()
check err pred = maybe (Right ()) (const $ Left err) . find pred

_newQuizSeriesStarted :: Event -> Maybe QuizConfig
_newQuizSeriesStarted = \case NewQuizSeriesStarted x -> Just x; _ -> Nothing
_quizSeriesEnded :: Event -> Maybe QuizStats
_quizSeriesEnded = \case QuizSeriesEnded x -> Just x; _ -> Nothing
_quizSolved :: Event -> Maybe SolvedQuiz
_quizSolved = \case QuizSolved x -> Just x; _ -> Nothing
_quizSended :: Event -> Maybe Quiz
_quizSended = \case QuizSended x -> Just x; _ -> Nothing