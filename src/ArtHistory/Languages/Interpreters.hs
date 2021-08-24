-- {-# OPTIONS_GHC -Weverything #-} -- , TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module ArtHistory.Languages.Interpreters
  ( AppL
  , evalAppL
  ) where

import ArtHistory.Languages.Definitions

import qualified ArtHistory.Domain as Domain
import ArtHistory.Messages (PolyShow(PShow), ShowFor(ForDebug))
import ArtHistory.Types (Artwork, Error(..), Event(..), Quiz, QuizConfig)
import Tools.Combinators (addToIORef)
import Types.Common as Common
  ( AppData(..)
  , Message(..)
  , Sub(..)
  , Subscription(..)
  )

import Resources as Res (artworks, randomQuizSet)

import Control.Concurrent.Chan (writeList2Chan)
import Control.Monad.Free (foldFree)
import Control.Monad.Reader (lift)
import Data.IORef (modifyIORef, readIORef)

import Control.Applicative (Applicative(liftA2))
import Control.Monad.Identity (Identity)
import Control.Monad.State (MonadTrans, get, gets, MonadIO (liftIO))
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import Discord (DiscordHandler, restCall)
import qualified Discord.Internal.Rest.Channel as RChann (ChannelRequest(..))
import qualified Discord.Internal.Types.Channel as TChann (Message(..))
import Optics (_Left, over)

evalAppL ::
     AppData (Sub Event) a -> Subscription -> AppL a b -> DiscordHandler b
evalAppL app sub appl = fst <$> runApp appl (app, sub)

-- newtype App s m a = App (s -> m (a,s))
type AppInfo a = (AppData (Sub Event) a, Subscription)

type AppL b = App (AppData (Sub Event) b, Subscription) DiscordHandler

getSubEventsHistory :: AppL b [Sub Event]
getSubEventsHistory = fromIO . readIORef =<< gets (_eventsHistory . fst)

fromIO :: IO a -> AppL b a
fromIO = lift . lift

instance AppM (AppL b) where
  subscriptionEvents :: AppL b [Event]
  subscriptionEvents =
    showBefore =<< liftA2 sub_events (gets snd) getSubEventsHistory
    where
      sub_events :: Subscription -> [Sub Event] -> [Event]
      sub_events sub =
        map _subscriptionStored . filter ((== sub) . _subscriptionInfo)

  unsolvedQuiz :: AppL b (Result Quiz)
  unsolvedQuiz = showBefore . Domain.unsolvedQuiz =<< subscriptionEvents

  quizConfig :: AppL b (Result QuizConfig)
  quizConfig = showBefore . Domain.quizConfig =<< subscriptionEvents

  pushEvents :: [Event] -> AppL b (Result ())
  pushEvents events = do
      (app, sub) <- get
      let sub_events = map (Sub sub) events
      fromIO $ addToIORef (_eventsHistory app) sub_events
      fromIO $ writeList2Chan (_eventsHub app) sub_events
        --print "Events added. Events now:"
        --print . map (PShow @ForDebug . _subscriptionStored) =<< readIORef (_eventsHistory app)
      pure $ Right ()

  sendMessage :: Message -> AppL b (Result ())
  sendMessage (Message msg (Subscription _ chann)) =
    either (Left . Error . show) (const $ Right ()) 
    <$> lift (restCall message)
    where
      message = RChann.CreateMessage chann msg

  randomQuizSet :: Int -> AppL b (Result (Artwork, NonEmpty Artwork))
  randomQuizSet n = over _Left (Error . show) <$> fromIO (Res.randomQuizSet n)
    where
      error = Error "Error: cant generate random quiz set"

showBefore :: Show a => a -> AppL b a
showBefore x = fromIO $ print x >> pure x
