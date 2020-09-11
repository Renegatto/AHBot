module ArtHistory.Types where
import Types.Common(Image,Message)

data Art = Art String deriving (Eq,Show)
data Artwork = Artwork 
    {
        artworkAuthor  :: String,
        artworkYear    :: String,
        artworkName    :: String,
        artworkImage   :: Image
    } deriving (Eq,Show)
data Variant = Variant {variantNumber::Int, variantArtwork::Artwork} deriving (Eq,Show)
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
    |Failed   Variant Quiz
    deriving (Eq,Show)
data QuizSeriesData = QuizSeriesData Variant
    

data Event =
    NewQuizSeriesStarted QuizConfig
    |QuizSended Quiz
    |QuizSolved SolvedQuiz
    |QuizSeriesEnded QuizStats
    |MessageSent Message
    deriving (Eq,Show)
data Command =
    NewQuizSeries QuizConfig
    |NextQuiz QuizConfig
    |SolveQuiz Quiz Variant
    |EndQuizSeries
    |SendMessage Message