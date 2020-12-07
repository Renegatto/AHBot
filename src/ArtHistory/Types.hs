module ArtHistory.Types where
import qualified Types.Common as Common (Image,Message)

newtype Art = Art String deriving (Eq,Show)
data Artwork = Artwork 
    {
        artworkAuthor  :: String,
        artworkYear    :: String,
        artworkName    :: String,
        artworkImage   :: Common.Image,
        artworkArt     :: Art
    } deriving (Eq,Show)
data Variant = Variant {variantNumber::Int, variantArtwork::Artwork} deriving (Eq,Show)
newtype Answer = Answer {answerVariant :: Int} deriving (Eq,Show)

data Quiz = Quiz Variant [Variant] deriving (Eq,Show)
data QuizConfig = QuizConfig 
    { cfgTotalVariants  :: Int
    , cfgArt            :: Art
    , cfgTotalQuizes    :: Int  
    } deriving (Eq,Show)
data QuizStats = QuizStats
    { statsPassed   :: Int
    , statsConfig   :: QuizConfig
    , statsSolveds  :: [SolvedQuiz]
    } deriving (Eq,Show)
data SolvedQuiz = 
    Succesful Variant Quiz
    |Failed   (Either Answer Variant) Quiz
    deriving (Eq,Show)

data Event =
    NewQuizSeriesStarted QuizConfig
    |QuizSended Quiz
    |QuizSolved SolvedQuiz
    |QuizSeriesEnded QuizStats
    |MessageSent Common.Message
    |DomainError Error
    deriving (Eq,Show)
data Command =
    NewQuizSeries QuizConfig
    |NextQuiz
    |SolveQuiz Answer
    |EndQuizSeries
    |SendMessage Common.Message deriving (Eq,Show)

newtype Error = Error String deriving (Eq,Show)
type Result a = Either Error a
