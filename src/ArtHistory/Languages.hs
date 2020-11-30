{-# LANGUAGE GADTs, DeriveFunctor #-}
module ArtHistory.Languages ( AppL
                            , subscriptionEvents
                            , unsolvedQuiz
                            , quizConfig
                            , pushEvents
                            , randomQuizSet
                            , QuizSet
                            , Result
                            , evalAppL ) where

import Types.Common     (Subscription,AppData)
import ArtHistory.Types (Event(..),Quiz,QuizConfig,Error,Artwork)

import Control.Monad.Free

type AppL a = Free App a
data App  a = RandApp (Random a) | EventApp (EventStorage a) deriving Functor


data EventStorage a = SubscriptionEvents ([Event] -> a)
                    | UnsolvedQuiz       (Result Quiz -> a)
                    | QuizConfig         (Result QuizConfig -> a)
                    | PushEvents         [Event] (Result () -> a)
                    deriving Functor
data Random a = RandomQuizSet Int (Result (Artwork,[Artwork]) -> a) deriving Functor

type QuizSet  = (Artwork,[Artwork])
type Result a = Either Error a

subscriptionEvents  :: AppL [Event]
unsolvedQuiz        :: AppL (Result Quiz)
quizConfig          :: AppL (Result QuizConfig)
pushEvents          :: [Event] -> AppL (Result ())

randomQuizSet :: Int -> AppL (Result (Artwork,[Artwork]))

subscriptionEvents  = liftF $ EventApp $ SubscriptionEvents id
unsolvedQuiz        = liftF $ EventApp $ UnsolvedQuiz id
quizConfig          = liftF $ EventApp $ QuizConfig id
pushEvents events   = liftF $ EventApp $ PushEvents events id

randomQuizSet n = liftF $ RandApp $ RandomQuizSet n id

evalEventStorage :: Subscription -> EventStorage a -> IO a
evalEventStorage = hole
evalAppL :: AppData Event a -> Subscription -> AppL b -> IO b
evalAppL = hole

hole = undefined