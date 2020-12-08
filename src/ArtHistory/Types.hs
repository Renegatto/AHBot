{-# LANGUAGE TemplateHaskell #-}
module ArtHistory.Types where
import Control.Lens
import qualified Types.Common as Common (Image,Message)

newtype Art = Art String deriving (Eq,Show)
newtype Error
    = Error String 
    deriving (Eq,Show)

data Artwork = Artwork 
    { _artworkAuthor  :: String
    , _artworkYear    :: String
    , _artworkName    :: String
    , _artworkImage   :: Common.Image
    , _artworkArt     :: Art
    } deriving (Eq,Show)
makeLenses ''Artwork
data Variant = Variant 
    { _variantNumber::Int
    , _variantArtwork::Artwork
    } deriving (Eq,Show)
makeLenses ''Variant
newtype Answer = Answer 
    { _answerVariant :: Int
    } deriving (Eq,Show)
makeLenses ''Answer
data Quiz
    = Quiz Variant [Variant] 
    deriving (Eq,Show)
data QuizConfig = QuizConfig 
    { _cfgTotalVariants  :: Int
    , _cfgArt            :: Art
    , _cfgTotalQuizes    :: Int  
    } deriving (Eq,Show)
makeLenses ''QuizConfig
data SolvedQuiz 
    = Succesful Variant Quiz
    | Failed   (Either Answer Variant) Quiz
    deriving (Eq,Show)
makePrisms ''SolvedQuiz
data QuizStats = QuizStats
    { _statsPassed   :: Int
    , _statsConfig   :: QuizConfig
    , _statsSolveds  :: [SolvedQuiz]
    } deriving (Eq,Show)
makeLenses ''QuizStats
data Event 
    = NewQuizSeriesStarted  QuizConfig
    | QuizSended            Quiz
    | QuizSolved            SolvedQuiz
    | QuizSeriesEnded       QuizStats
    | MessageSent           Common.Message
    | DomainError           Error
    deriving (Eq,Show)
makePrisms ''Event
data Command 
    = NewQuizSeries QuizConfig
    | NextQuiz
    | SolveQuiz      Answer
    | EndQuizSeries
    | SendMessage    Common.Message 
    deriving (Eq,Show)
makePrisms ''Command


 
type Result a = Either Error a
