{-# LANGUAGE GADTs, FlexibleInstances #-}

module ArtHistory.Events where
import ArtHistory.Types
import qualified ArtHistory.Domain as Domain
import Resources (randomQuizSet)

import Data.Functor.Compose
import Data.Functor(($>))
import Control.Monad(void,mapM_,join,(<=<))
import Data.Maybe(fromMaybe,maybeToList)
import Data.List(find)
import Data.Foldable(foldMap)

import Discord (DiscordHandler,restCall)
import Discord.Internal.Rest.Channel as RChan
import Discord.Internal.Types (ChannelId)

import Control.Monad.Reader(ReaderT(..))
import qualified Data.Text as T

eventAction :: ChannelId -> Event -> DiscordHandler ()
eventAction discord_channel =
    void . restCall 
    . RChan.CreateMessage discord_channel 
    . T.pack . show
        
nextQuiz :: ChannelId -> QuizConfig -> DiscordHandler [Event]
nextQuiz discord_channel cfg =
    handle_events <=< ReaderT . const
    . fmap (join . maybeToList) . getCompose
    . fmap (Domain.nextQuiz cfg ) 
    . randomQuizSet 
    $ cfgTotalVariants cfg
    where
    handle_events :: [Event] -> DiscordHandler [Event]
    handle_events = ($>) =<< mapM_ (eventAction discord_channel)

cmd dc (NextQuiz cfg) = nextQuiz dc cfg




newtype MessageContent a = MessageContent a
instance Show (MessageContent Variant) where
    show (MessageContent (Variant n (Artwork author _ name _))) =
        show n <> ". " <> author <> " " <> name
instance Show (MessageContent Event) where
    show (MessageContent (QuizSended (Quiz _ variants))) =
        "Variants: "
        <> foldMap (mappend "\n" . show . MessageContent) variants 
    show (MessageContent (NewQuizSeriesStarted (QuizConfig 
            variants
            art
            quizes 
        ))) =
        "You were started the quiz series about art of " 
        <> show art         <> ", from " 
        <> show quizes      <> " quizes, with " 
        <> show variants    <> " variants per quiz."
    show (MessageContent (QuizSolved (Failed answer (Quiz right _)))) =
        "Failed. You answered " <> show (MessageContent answer)
        <> "but it were" <> show right
    show (MessageContent (QuizSolved (Succesful answer _))) =
        "Right! It is really " <> show answer    
    