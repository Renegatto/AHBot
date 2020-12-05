{-# LANGUAGE GADTs, FlexibleInstances #-}

module ArtHistory.Messages where 
import ArtHistory.Types
newtype MessageContent a = MessageContent a
instance Show (MessageContent Variant) where
    show (MessageContent (Variant n (Artwork author _ name _))) =
        show n <> ". " <> author <> " " <> name
instance Show (MessageContent Answer) where
    show (MessageContent (Answer n )) = show n
instance Show (MessageContent QuizConfig) where
    show (MessageContent (QuizConfig 
        variants art quizes )) =

        foldMap (<> "\n")
        ["The art: "            <> show art
        ,"Quiz count is :"      <> show quizes 
        ,"Variants per quiz: "  <> show variants]

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
        <> "You succefly finished " <> show passed <>
        "quizes,\n"
    show (MessageContent (MessageSent text)) =
        show text
    show (MessageContent (DomainError (Error e))) = e 