module ArtHistory.Types where
data Art = Art String
data Artwork = Artwork 
    {
        artworkAuthor  :: String,
        artworkYear    :: String,
        artworkName    :: String
    }
data Variant = Variant {variantNumber::Int, variantArtwork::Artwork}
data Quiz = Quiz Variant [Variant]
data QuizConfig = QuizConfig 
    { cfgTotalVariants  :: Int
    , cfgArt            :: Art
    , cfgTotalQuizes    :: Int
    }
data QuizStats = QuizStats
    { statsPassed   :: Int
    , statsConfig   :: QuizConfig
    , statsSolveds  :: [SolvedQuiz]
    }
data SolvedQuiz = 
    Succesful Variant Quiz
    |Failed   Variant Quiz
 


data Event =
    NewQuizSeriesStarted QuizConfig
    |QuizSended Quiz
    |QuizSolved SolvedQuiz
    |TestEnded QuizStats
data Command =
    NewQuizSeries QuizConfig
    |NextQuiz QuizConfig
    |SolveQuiz Quiz Variant
    |EndQuiz