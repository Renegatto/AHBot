{-# LANGUAGE GADTs, FlexibleInstances
, GeneralizedNewtypeDeriving, FlexibleContexts
, DeriveTraversable, DataKinds, KindSignatures, ExplicitForAll
, TemplateHaskell, TypeApplications #-}
module ArtHistory.Messages where 
import ArtHistory.Types
import Types.Common (Message(..),Image(..),msg)
import Data.List (intersperse)
import Data.Foldable (fold)
import Text.Show.Unicode(ushow)
import Control.Lens

data ShowFor = ForDebug | ForMessage
newtype PolyShow (a :: ShowFor) b = PShow {_pshow :: b}
makeLenses ''PolyShow

nl = ['\n']

--newtype MessageContent a = MessageContent a deriving (Functor,Foldable,Traversable)

instance Show (PolyShow ForDebug Message) where
    show = show . (^. pshow . msg)
type Debug a = PolyShow ForDebug a

instance Show (Debug Event) where
    show (PShow (NewQuizSeriesStarted (QuizConfig vars (Art art) quizes))) = 
        "NewQuizSeriesStarted " <> fold (intersperse " " [show vars,show art,show quizes]) <> nl
    show (PShow (QuizSended quiz)) = 
        "QuizSended" <> nl
    show (PShow (QuizSolved quiz)) = 
        "QuizSolved" <> nl
    show (PShow (QuizSeriesEnded _)) = 
        "QuizSeriesEnded" <> nl
    show (PShow (MessageSent msg)) = 
        "MessageSent " <> show (PShow msg :: Debug Message) <> nl
    show (PShow (DomainError (Error e))) = 
        "DomainError " <> e <> nl
instance Show (Debug Command) where
    show (PShow (NewQuizSeries (QuizConfig vars (Art art) quizes))) = 
        "NewQuizSeries " <> fold (intersperse " " [show vars,show art,show quizes]) <> nl
    show (PShow NextQuiz) = 
        "NextQuiz" <> nl
    show (PShow (SolveQuiz quiz)) = 
        "SolveQuiz" <> nl
    show (PShow EndQuizSeries) = 
        "EndQuizSeries" <> nl
    show (PShow (SendMessage msg)) = 
        "SendMessage " <> show (PShow msg :: Debug Message) <> nl

type MessageCont a = PolyShow ForMessage a

showm :: Show (MessageCont a) => a -> String
showm = show . PShow @ForMessage
showm1 :: Show (MessageCont a) => MessageCont a -> String
showm1 = show

instance Show (MessageCont Art) where
    show (PShow (Art artname)) = "The art of " <> ushow artname
instance Show (MessageCont Variant) where
    show (PShow (Variant n (Artwork author _ name _ art))) =
        "Variant " <> show n <> " : " <> nl 
        <> showm art <> nl 
        <> author <> nl 
        <> name   <> nl 
instance Show (MessageCont Answer) where
    show = show.(^.pshow.answerVariant)
instance Show (MessageCont QuizConfig) where
    show (PShow (QuizConfig 
        variants art quizes )) =

        foldMap (<> nl)
        ["The art: "            <> show art
        ,"Quiz count is :"      <> show quizes 
        ,"Variants per quiz: "  <> show variants]
instance Show (MessageCont Image) where
    show (PShow (Image url)) = url
instance Show (MessageCont SolvedQuiz) where
    show (PShow (Succesful h x)) = show x <> nl <> show h
    show (PShow (Failed    h x)) = show x <> nl <> show h
instance Show (MessageCont Event) where
    show (PShow (QuizSended (Quiz (Variant _ artwork) variants))) =
        "The artwork: " <> showm image
        <> nl <> "Variants: " <> nl
        <> foldMap (mappend nl . showm) variants 
        where image = _artworkImage artwork
    show (PShow (NewQuizSeriesStarted (QuizConfig 
            variants
            art
            quizes 
        ))) =
        "You were started the quiz series about art of " 
        <> show art         <> ", from " 
        <> show quizes      <> " quizes, with " 
        <> show variants    <> " variants per quiz."
    show (PShow (QuizSolved (Failed answer (Quiz right _)))) =
        "Failed. You answered " 
        <> show (either showm showm answer)
        <> "but it were" <> show right
    show (PShow (QuizSolved (Succesful answer _))) =
        "Right! It is really " <> show answer
    show (PShow (QuizSeriesEnded (
        QuizStats passed cfg answers
        )))  =
        "You just done completing the quiz series:\n" 
        <> showm cfg <> "\n"
        <> "You successfully finished " <> show passed <>
        "quizes," <> nl
        <> foldMap showm  answers
    show (PShow (MessageSent text)) =
        show text
    show (PShow (DomainError (Error e))) = e