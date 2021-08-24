{-# LANGUAGE GADTs, DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module ArtHistory.Languages.Definitions where
import           ArtHistory.Messages
import           Types.Common        as Common (Message)
import           ArtHistory.Types              (Event,Quiz,QuizConfig,Error,Artwork)
import qualified ArtHistory.Domain   as Domain

import           Control.Monad.Free            (Free,liftF)
import           Control.Monad.State.Lazy (MonadState, StateT(..), MonadIO)
import           Data.IORef                    (modifyIORef,readIORef)
import           Data.List.NonEmpty (NonEmpty)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Writer.Strict (MonadTrans)
import Discord (DiscordHandler)

--type AppL a = App () Identity a

type QuizSet  = (Artwork,[Artwork])
type Result a = Either Error a

class Monad m => AppM m where
    subscriptionEvents  :: m [Event]
    unsolvedQuiz        :: m (Result Quiz)
    quizConfig          :: m (Result QuizConfig)
    pushEvents          :: [Event] -> m (Result ())

    sendMessage         :: Message -> m (Result ())

    randomQuizSet       :: Int -> m (Result (Artwork,NonEmpty Artwork))

newtype App s m a = App {runApp :: s -> m (a,s)}
    deriving stock Functor
    deriving 
        (Applicative
        , Monad
        , MonadState s
        , MonadReader s) via (StateT s m)
    deriving MonadTrans via (StateT s)


