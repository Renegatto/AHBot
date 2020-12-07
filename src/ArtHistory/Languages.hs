{-# LANGUAGE GADTs, DeriveFunctor #-}
module ArtHistory.Languages ( AppL
                            , subscriptionEvents
                            , unsolvedQuiz
                            , quizConfig
                            , pushEvents
                            , randomQuizSet
                            , sendMessage
                            , QuizSet
                            , Result
                            , evalAppL ) where
import ArtHistory.Messages
import Types.Common as Common (Subscription,AppData(..),Sub(..),Message(..),Subscription(..))
import ArtHistory.Types (Event(..),Quiz,QuizConfig,Error(..),Artwork,Command)
import Resources (randomQuizSet')
import qualified ArtHistory.Domain as Domain

import Data.IORef(modifyIORef,readIORef)
import Control.Monad.Free
import Data.Foldable (traverse_)
import Control.Monad.Reader(lift)
import Control.Concurrent.Chan (writeList2Chan)
import           Discord (DiscordHandler,restCall,RestCallErrorCode,DiscordHandle)
import qualified Discord.Internal.Rest.Channel as RChann (ChannelRequest(..))
import qualified Discord.Internal.Types.Channel as TChann (Message(..))
type AppL a = Free App a
data App  a = RandApp (Random a) | EventApp (EventStorage a) | DiscordApp (DiscordApp a) deriving Functor

data DiscordApp a = SendMessage Message (Result () -> a) deriving Functor

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
sendMessage         :: Message -> AppL (Result ())

randomQuizSet :: Int -> AppL (Result (Artwork,[Artwork]))

subscriptionEvents  = liftF $ EventApp $ SubscriptionEvents id
unsolvedQuiz        = liftF $ EventApp $ UnsolvedQuiz id
quizConfig          = liftF $ EventApp $ QuizConfig id
pushEvents events   = liftF $ EventApp $ PushEvents events id

randomQuizSet n = liftF $ RandApp $ RandomQuizSet n id

sendMessage msg = liftF $ DiscordApp $ SendMessage msg id

evalRandom :: Random a -> IO a
evalRandom (RandomQuizSet n cont) = 
    cont . maybe (Left error) Right 
    <$> randomQuizSet' n
    where error = Error "Error: cant generate random quiz set"

showBefore x = print x >> pure x

evalDiscordApp :: DiscordApp a -> DiscordHandler a
evalDiscordApp (SendMessage (Message msg (Subscription sub chann)) cont) = 
        cont . either (Left . Error . show) (const $ Right ())
        <$> (restCall message :: DiscordHandler (Either RestCallErrorCode TChann.Message))
        where
        message = RChann.CreateMessage chann msg

evalEventStorage :: AppData (Sub Event) a -> Subscription -> EventStorage b -> IO b
evalEventStorage app sub (SubscriptionEvents cont) =
    cont <$> (showBefore =<< this_step)
    where 
    this_step =
        map subscriptionStored 
        . filter ((== sub) . subscriptionInfo)
        <$> readIORef (eventsHistory app)
evalEventStorage app sub (UnsolvedQuiz cont) = 
    cont <$> (showBefore =<< this_step)
    where 
    this_step = 
        evalEventStorage app sub 
        $ SubscriptionEvents Domain.unsolvedQuiz
evalEventStorage app sub (QuizConfig cont) =
    cont <$> (showBefore =<< this_step)
    where
    this_step = 
        evalEventStorage app sub 
        $ SubscriptionEvents Domain.quizConfig    
evalEventStorage app sub (PushEvents events cont) = do
    modifyIORef (eventsHistory app) (sub_events ++) 
    writeList2Chan (eventsHub app)  sub_events
    print "Events added. Events now:"
    print . map (Debug . subscriptionStored)  =<< readIORef (eventsHistory app)
    pure $ cont $ Right ()
    where sub_events = map (Sub sub) events

evalApp :: AppData (Sub Event) a -> Subscription -> App b -> DiscordHandler b
evalApp app sub (RandApp  x) = lift $ evalRandom x
evalApp app sub (EventApp x) = lift $ evalEventStorage app sub x
evalApp app sub (DiscordApp x) = evalDiscordApp x
evalAppL :: AppData (Sub Event) a -> Subscription -> AppL b -> DiscordHandler b
evalAppL app sub = foldFree $ evalApp app sub

--hole = undefined