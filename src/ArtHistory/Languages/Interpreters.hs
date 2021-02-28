{-# LANGUAGE LambdaCase, DataKinds #-} -- , TypeApplications #-}
module ArtHistory.Languages.Interpreters (AppL,evalAppL) where

import ArtHistory.Languages.Definitions

import           ArtHistory.Messages                      (PolyShow(PShow),ShowFor(ForDebug))
import           Types.Common                   as Common (AppData(..),Sub(..),Message(..),Subscription(..))
import           ArtHistory.Types                         (Event(..),QuizConfig,Error(..))
import qualified ArtHistory.Domain              as Domain
import           Tools.Combinators                        (addToIORef)

import           Resources                      as Res    (randomQuizSet,artworks)

import           Control.Monad.Free                       (foldFree)
import           Control.Monad.Reader                     (lift)
import           Control.Concurrent.Chan                  (writeList2Chan)
import           Data.IORef                               (modifyIORef,readIORef)

import           Discord                                  (DiscordHandler,restCall)
import qualified Discord.Internal.Rest.Channel  as RChann (ChannelRequest(..))
import qualified Discord.Internal.Types.Channel as TChann (Message(..))
import Optics (over,_Left)

evalAppL :: AppData (Sub Event) a -> Subscription -> AppL b -> DiscordHandler b
evalAppL app sub = foldFree $ evalApp app sub

evalApp :: AppData (Sub Event) a -> Subscription -> App b -> DiscordHandler b
evalApp app sub = \case
    RandApp     program -> lift $ evalRandom program
    EventApp    program -> lift $ evalEventStorage app sub program
    DiscordApp  program -> evalDiscordApp program

evalRandom :: Random a -> IO a
evalRandom (RandomQuizSet n cont) = 
    cont . over _Left (Error . show) <$> Res.randomQuizSet n
    where error = Error "Error: cant generate random quiz set"

evalDiscordApp :: DiscordApp a -> DiscordHandler a
evalDiscordApp (SendMessage (Message msg (Subscription sub chann)) cont) = 
    cont . either (Left . Error . show) (const $ Right ())
    <$> restCall message
    where
    message = RChann.CreateMessage chann msg

evalEventStorage :: AppData (Sub Event) a -> Subscription -> EventStorage b -> IO b
evalEventStorage app sub (SubscriptionEvents cont) =
    cont <$> (showBefore =<< this_step)
    where 
    this_step =
        map _subscriptionStored 
        . filter ((== sub) . _subscriptionInfo)
        <$> readIORef (_eventsHistory app)
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
    addToIORef (_eventsHistory app)  sub_events 
    writeList2Chan (_eventsHub app)  sub_events
    --print "Events added. Events now:"
    --print . map (PShow @ForDebug . _subscriptionStored)  =<< readIORef (_eventsHistory app)
    pure $ cont $ Right ()
    where sub_events = map (Sub sub) events

showBefore x = print x >> pure x