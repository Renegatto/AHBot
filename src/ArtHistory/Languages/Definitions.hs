{-# LANGUAGE GADTs, DeriveFunctor #-}
module ArtHistory.Languages.Definitions where
import           ArtHistory.Messages
import           Types.Common        as Common (Message)
import           ArtHistory.Types              (Event,Quiz,QuizConfig,Error,Artwork)
import qualified ArtHistory.Domain   as Domain

import           Control.Monad.Free            (Free,liftF)
import           Data.IORef                    (modifyIORef,readIORef)

type AppL a = Free App a
data App  a = RandApp    (Random       a) 
            | EventApp   (EventStorage a) 
            | DiscordApp (DiscordApp   a) deriving Functor

data DiscordApp a = SendMessage Message (Result () -> a) deriving Functor

data EventStorage a = SubscriptionEvents ([Event]           -> a)
                    | UnsolvedQuiz       (Result Quiz       -> a)
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

sendMessage         :: Message -> AppL (Result ())

randomQuizSet       :: Int -> AppL (Result (Artwork,[Artwork]))

subscriptionEvents  = liftF $ EventApp $ SubscriptionEvents id
unsolvedQuiz        = liftF $ EventApp $ UnsolvedQuiz       id
quizConfig          = liftF $ EventApp $ QuizConfig         id
pushEvents events   = liftF $ EventApp $ PushEvents events  id

sendMessage msg     = liftF $ DiscordApp $ SendMessage msg  id

randomQuizSet n     = liftF $ RandApp $ RandomQuizSet   n   id

