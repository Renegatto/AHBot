{-# LANGUAGE ScopedTypeVariables #-} --InstanceSigs, 
module ArtHistory.Types where
import qualified Types.Common as Common (Image,Message,AppData(..),Sub(..))
import Control.Monad.Reader as Re

data Art = Art String deriving (Eq,Show)
data Artwork = Artwork 
    {
        artworkAuthor  :: String,
        artworkYear    :: String,
        artworkName    :: String,
        artworkImage   :: Common.Image
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
data Event =
    NewQuizSeriesStarted QuizConfig
    |QuizSended Quiz
    |QuizSolved SolvedQuiz
    |QuizSeriesEnded QuizStats
    |MessageSent Common.Message
    deriving (Eq,Show)
data Command =
    NewQuizSeries QuizConfig
    |NextQuiz
    |SolveQuiz Variant
    |EndQuizSeries
    |SendMessage Common.Message

newtype Error = Error String
data Result a = Ok a | Fail Error

type App = Common.AppData (Common.Sub Event) (Common.Sub Command)

newtype AppContext a m b = AppContext (Re.ReaderT (App,Common.Sub a) m (Common.Sub (Result b)))
readContext = AppContext . ReaderT

readContext' :: (Monad m) => ((App,Common.Sub a) -> m (Result b)) -> AppContext a m b
readContext' f = 
    AppContext $ ReaderT $ \(app,sub) -> (<$ sub) <$> f (app,sub) 

runContext :: (Monad m) => AppContext a m b -> (App,Common.Sub a) -> m (Common.Sub (Result b))
runContext (AppContext r) = runReaderT r

instance (Monad m) => Functor (AppContext a m) where
    --fmap :: forall a m b e. (a -> b) -> AC e m a -> AC e m b
    fmap f (AppContext context) = 
        AppContext (fmap (fmap f) <$> context)
instance Functor Result where
    fmap f (Ok x) = Ok $ f x
    fmap _ (Fail x) = Fail x
instance Applicative Result where
    pure x = Ok x
    (Ok f)   <*> (Ok x)   = Ok (f x)
    (Fail e) <*> _        = Fail e
    _        <*> (Fail e) = Fail e
instance  (Monad m) => Applicative (AppContext a m) where
   -- pure :: forall a m b. b -> AppContext a m b
    pure b = AppContext (Re.ReaderT fn)
        where 
        --fn :: (App,Common.Sub a) -> m (Common.Sub (Result b))
        fn = \(_,f) -> pure (Ok b <$ f)
   -- (<*>) :: forall a m b c. AC a m (b -> c) -> AC a m b -> AC a m c
    (AppContext fctx) <*> (AppContext (actx :: R a m (SP b))) =
        AppContext 
            (actx `goo` fctx)
        where
        --goo :: R a m (SP b) -> R a m (SP (b -> c)) -> R a m (SP c)
        goo x y = fmap appl x <*> y

        --appl :: forall a b. SP a -> SP (a -> b) -> SP b
        appl (Common.Sub _ a) (Common.Sub sub f) = Common.Sub sub (f <*> a)

type AC a m b = AppContext a m b
type S a = Common.Sub a
type P a = Result a
type R a m b = ReaderT (App,Common.Sub a) m b
type SP x = S (P x)

instance (Monad m) => Monad (AppContext a m) where
    (AppContext ctx) >>= mf =
        AppContext (ctx `f` flip s g) -- b -> (r -> m (Res c))
        where                            -- a -> m (Res b)                                 
        unwrap (AppContext x) = x        -- a -> m (Res c)
        --f :: R e IO (SP a) -> (SP a -> R e IO (SP b)) -> R e IO (SP b)
        f = (Prelude.>>=)
        --g :: a -> R e IO (SP b)
        g = unwrap . mf
        --s :: SP a -> (a -> R e IO (SP b)) -> R e IO (SP b)
        s (Common.Sub sub (Ok a)) f = f a
        s (Common.Sub sub (Fail e)) f = pure $ Common.Sub sub (Fail e)

