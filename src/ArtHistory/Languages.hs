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

import Types.Common     (Subscription,AppData(..),Sub(..))
import ArtHistory.Types (Event(..),Quiz,QuizConfig,Error(..),Artwork)
import Resources (randomQuizSet')
import Control.Monad.Free
import Control.Concurrent.Chan (writeList2Chan,getChanContents)
import qualified ArtHistory.Domain as Domain

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

evalRandom :: Random a -> IO a
evalRandom (RandomQuizSet n cont) = 
    cont . maybe (Left error) Right 
    <$> randomQuizSet' n
    where error = Error "Error: cant generate random quiz set"

evalEventStorage :: AppData (Sub Event) a -> Subscription -> EventStorage b -> IO b
evalEventStorage app sub (SubscriptionEvents cont) = do
    cont . map subscriptionStored 
    . filter ((== sub) . subscriptionInfo)
    <$> getChanContents (eventsHistory app)
evalEventStorage app sub (UnsolvedQuiz cont) = 
    fmap cont 
    $ evalEventStorage app sub 
    $ SubscriptionEvents Domain.unsolvedQuiz
evalEventStorage app sub (QuizConfig cont) =
    fmap cont 
    $ evalEventStorage app sub 
    $ SubscriptionEvents Domain.quizConfig    
evalEventStorage app sub (PushEvents events cont) = do
    writeList2Chan (eventsHistory app) $ map (Sub sub) events
    pure (cont $ pure ())

evalApp :: AppData (Sub Event) a -> Subscription -> App b -> IO b
evalApp app sub (RandApp  x) = evalRandom x
evalApp app sub (EventApp x) = evalEventStorage app sub x

evalAppL :: AppData (Sub Event) a -> Subscription -> AppL b -> IO b
evalAppL app sub = foldFree $ evalApp app sub

--hole = undefined