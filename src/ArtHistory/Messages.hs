{-# LANGUAGE GADTs, FlexibleInstances #-}

module ArtHistory.Messages where 
import ArtHistory.Types
import Types.Common (Message(..),Image(..))
import Data.List (intersperse)
import Data.Foldable (fold)
import Text.Show.Unicode(ushow)
newtype MessageContent a = MessageContent a

newtype Debug a = Debug a
instance Show (Debug Message) where
    show (Debug (Message msg to)) = show msg
nl = ['\n']
instance Show (Debug Event) where
    show (Debug (NewQuizSeriesStarted (QuizConfig vars (Art art) quizes))) = 
        "NewQuizSeriesStarted " <> fold (intersperse " " [show vars,show art,show quizes]) <> nl
    show (Debug (QuizSended quiz)) = 
        "QuizSended" <> nl
    show (Debug (QuizSolved quiz)) = 
        "QuizSolved" <> nl
    show (Debug (QuizSeriesEnded stats)) = 
        "QuizSeriesEnded" <> nl
    show (Debug (MessageSent msg)) = 
        "MessageSent " <> show (Debug msg) <> nl
    show (Debug (DomainError (Error e))) = 
        "DomainError " <> e  <> nl
instance Show (Debug Command) where
    show (Debug (NewQuizSeries (QuizConfig vars (Art art) quizes))) = 
        "NewQuizSeries " <> fold (intersperse " " [show vars,show art,show quizes]) <> nl
    show (Debug NextQuiz) = 
        "NextQuiz" <> nl
    show (Debug (SolveQuiz quiz)) = 
        "SolveQuiz" <> nl
    show (Debug EndQuizSeries) = 
        "EndQuizSeries" <> nl
    show (Debug (SendMessage msg)) = 
        "SendMessage " <> show (Debug msg) <> nl

instance Show (MessageContent Art) where
    show (MessageContent (Art artname)) = "The art of " <> ushow artname
instance Show (MessageContent Variant) where
    show (MessageContent (Variant n (Artwork author _ name _ art))) =
        "Variant " <> show n <> " : " <> nl 
        <> show (MessageContent art) <> nl 
        <> author <> nl 
        <> name   <> nl 
instance Show (MessageContent Answer) where
    show (MessageContent (Answer n )) = show n
instance Show (MessageContent QuizConfig) where
    show (MessageContent (QuizConfig 
        variants art quizes )) =

        foldMap (<> nl)
        ["The art: "            <> show art
        ,"Quiz count is :"      <> show quizes 
        ,"Variants per quiz: "  <> show variants]
instance Show (MessageContent Image) where
    show (MessageContent (Image url)) = url
instance Show (MessageContent Event) where
    show (MessageContent (QuizSended (Quiz (Variant _ artwork) variants))) =
        "The artwork: " <> show (MessageContent image)
        <> nl <> "Variants: " <> nl
        <> foldMap (mappend nl . show . MessageContent) variants 
        where image = artworkImage artwork
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
        "Failed. You answered " 
        <> show (either (show . MessageContent) (show . MessageContent) answer)
        <> "but it were" <> show right
    show (MessageContent (QuizSolved (Succesful answer _))) =
        "Right! It is really " <> show answer
    show (MessageContent (QuizSeriesEnded (
        QuizStats passed cfg _
        )))  =
        "You just done completing the quiz series:\n" 
        <> show (MessageContent cfg) <> "\n"
        <> "You successfully finished " <> show passed <>
        "quizes,\n"
    show (MessageContent (MessageSent text)) =
        show text
    show (MessageContent (DomainError (Error e))) = e 