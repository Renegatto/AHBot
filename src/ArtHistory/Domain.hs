module ArtHistory.Domain where

import ArtHistory.Types
import Control.Monad ((<=<), (>=>))
import Data.List (find)
import Data.List.NonEmpty as NE (NonEmpty(..), zipWith)
import Data.Maybe (isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Optics
  ( Each(each)
  , Prism'
  , Swapped(swapped)
  , (%)
  , (%~)
  , (^?)
  , _head
  , afolding
  , headOf
  , only
  , preview
  , view )
import Tools.Combinators ((...))

newQuizSeries :: QuizConfig -> [Event]
newQuizSeries cfg = [NewQuizSeriesStarted cfg]

nextQuiz :: QuizConfig -> (Artwork, NonEmpty Artwork) -> [Event]
nextQuiz cfg (art, arts) = maybeToList (QuizSended <$> try_quiz)
  where
    variants = NE.zipWith Variant (0 :| [1 ..]) arts
    try_quiz = flip Quiz variants <$> try_right_variant variants
    try_right_variant = find ((art ==) . _variantArtwork) 

foo art = only art

solveQuiz :: Answer -> Quiz -> [Event]
solveQuiz ans@(Answer answer) quiz@(Quiz right variants)
  | answer == _variantNumber right = [QuizSolved $ Succesful right quiz]
  | otherwise =
    case find ((== answer) . _variantNumber) variants of
      Just variant -> [QuizSolved $ Failed (Right variant) quiz]
      Nothing      -> [QuizSolved $ Failed (Left ans)      quiz]

fooo = headOf (each % only (3 :: Int)) [2 .. 100]

-- we need some prisms here
unsolvedQuiz :: [Event] -> Either Error Quiz
unsolvedQuiz events = notEnded after >> solved after >> sended before
  where
    (after, before) = span (isNothing . preview _QuizSended) events

    sended :: [Event] -> Either Error Quiz
    sended = tryUnpackHead (Error "No quizes has been sent") _QuizSended

    notEnded :: [Event] -> Either Error ()
    notEnded = notHappened
      (Error "Quiz series already ended")
      (isJust . preview _QuizSeriesEnded)

    solved :: [Event] -> Either Error ()
    solved = notHappened
      (Error "Quiz is already solved")
      (isJust . preview _QuizSolved)

endQuizSeries :: [Event] -> [Event]
endQuizSeries = pure . either DomainError QuizSeriesEnded . quizStats

quizStats :: [Event] -> Either Error QuizStats
quizStats events =
  started before 
  >> notEnded after 
  >> calcQuizStats (results after) <$> started before
  where
    (after, before) = span (isNothing . preview _NewQuizSeriesStarted) events
    results = mapMaybe (preview _QuizSolved)
    started = tryUnpackHead 
      (Error "No quizes has been started") 
      _NewQuizSeriesStarted
    notEnded = notHappened
      (Error "Quiz series already ended")
      (isJust . preview _QuizSeriesEnded)

calcQuizStats :: [SolvedQuiz] -> QuizConfig -> QuizStats
calcQuizStats results cfg = QuizStats
    { _statsPassed  = length results
    , _statsConfig  = cfg
    , _statsSolveds = results }

quizConfig :: [Event] -> Either Error QuizConfig
quizConfig events = notEnded after >> started before
  where
    (after, before) = span (isNothing . preview _NewQuizSeriesStarted) events
    started = tryUnpackHead 
      (Error "No quizes has been started") 
      _NewQuizSeriesStarted
    notEnded = notHappened
      (Error "Quiz series already ended")
      (isJust . preview _QuizSeriesEnded)

tryUnpackHead :: e -> Prism' a b -> [a] -> Either e b
tryUnpackHead e f = maybe (Left e) Right . preview (_head % f)

notHappened :: e -> (a -> Bool) -> [a] -> Either e ()
notHappened e = maybe (Right ()) (const $ Left e) ... find

happened :: e -> (a -> Bool) -> [a] -> Either e a
happened e = maybe (Left e) Right ... find
