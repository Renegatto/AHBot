{-# LANGUAGE TupleSections, TemplateHaskell #-} --InstanceSigs,
module ArtHistory.Types where
import Control.Lens
import qualified Types.Common as Common (Image,Message,AppData(..),Sub(..))
import Control.Monad.Reader as Re
import qualified Control.Category as Cat
import qualified Control.Arrow as Arr
newtype Art = Art String deriving (Eq,Show)
data Artwork = Artwork 
    {
        artworkAuthor  :: String,
        artworkYear    :: String,
        artworkName    :: String,
        artworkImage   :: Common.Image
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
    deriving (Eq,Show)
makeLenses ''Event
data Command =
    NewQuizSeries QuizConfig
    |NextQuiz
    |SolveQuiz Answer
    |EndQuizSeries
    |SendMessage Common.Message

newtype Error = Error String
type Result a = Either Error a

type App = Common.AppData (Common.Sub Event) (Common.Sub Command)
data AppC a = AppC {_app :: App, _value :: Common.Sub a}
newtype AppContext m a b = AppContext (Re.ReaderT (App,Common.Sub a) m (Common.Sub (Result b)))
readContext = AppContext . ReaderT

--toContext :: (Monad m) => (a -> b) -> AppContext m a b
--toContext f = 

readContext' :: (Monad m) => ((App,Common.Sub a) -> m (Result b)) -> AppContext m a b
readContext' f = 
    AppContext $ ReaderT $ \(app,sub) -> (<$ sub) <$> f (app,sub) 

runContext :: (Monad m) => AppContext m a b -> (App,Common.Sub a) -> m (Common.Sub (Result b))
runContext (AppContext r) = runReaderT r
{-
instance (Monad m) => Functor (AppContext m a) where
    --fmap :: forall a m b e. (a -> b) -> AC e m a -> AC e m b
    fmap f (AppContext context) = 
        AppContext $ (fmap . fmap . fmap) f context
{-instance Functor Result where
    fmap f (Right x) = Ok $ f x
    fmap _ (Left x) = Fail x
instance Applicative Result where
    pure = Ok
    (Right f)   <*> (Right x)   = Ok (f x)
    (Left e) <*> _        = Fail e
    _        <*> (Left e) = Fail e-}
instance  (Monad m) => Applicative (AppContext m a) where
   -- pure :: forall a m b. b -> AppContext a m b
    pure b = AppContext (Re.ReaderT fn)
        where 
        --fn :: (App,Common.Sub a) -> m (Common.Sub (Result b))
        fn = \(_,f) -> pure (Right b <$ f)
   -- (<*>) :: forall a m b c. AC a m (b -> c) -> AC a m b -> AC a m c
    (AppContext fctx) <*> (AppContext (actx :: R a m (SP b))) =
        AppContext (actx `goo` fctx)
        where
        --goo :: R a m (SP b) -> R a m (SP (b -> c)) -> R a m (SP c)
        goo = (<*>) . fmap appl

        --appl :: forall a b. SP a -> SP (a -> b) -> SP b
        appl (Common.Sub _ a) (Common.Sub sub f) = Common.Sub sub (f <*> a)

type AC a m b = AppContext m a b
type S a = Common.Sub a
type P a = Result a
type R a m b = ReaderT (App,Common.Sub a) m b
type SP x = S (P x)

instance (Monad m) => Monad (AppContext m a) where
    (AppContext ctx) >>= mf =
        AppContext (ctx `f` flip s g) -- b -> (r -> m (Res c))
        where                            -- a -> m (Res b)                                 
        unwrap (AppContext x) = x        -- a -> m (Res c)
        --f :: R e IO (SP a) -> (SP a -> R e IO (SP b)) -> R e IO (SP b)
        f = (Prelude.>>=)
        --g :: a -> R e IO (SP b)
        g = unwrap . mf
        --s :: SP a -> (a -> R e IO (SP b)) -> R e IO (SP b)
        s (Common.Sub sub (Right a)) f = f a
        s (Common.Sub sub (Left e)) f = pure $ Common.Sub sub (Left e)

instance (Monad m) => Cat.Category (AppContext m) where
    id = AppContext $ ReaderT (\(app,x) -> pure (Right <$> x))
    (AppContext (ReaderT ctx)) . (AppContext (ReaderT ctx')) = 
        AppContext $ ReaderT (composition ctx ctx')
        where

        result2 app r1 f = do
            Common.Sub sub res1 <- r1
            case res1 of
                Right b -> f (app,Common.Sub sub b)
                Left  e -> pure $ Common.Sub sub (Left e)

        composition f g arg@(app, Common.Sub sub x) =
            result2 app (g arg) f
--  (.<) :: (Monad m) => AppContext m b c -> AppContext m a b -> AppContext m a c
--  (.<) = (Cat..)
instance (Monad m) => Arr.Arrow (AppContext m) where
    arr f = readContext' (\(_,Common.Sub sub x) -> pure $ Right $ f x)
    first (AppContext (ReaderT ctx)) = readContext $ foo ctx
        where
        foo ctx' input@(_,Common.Sub _ (a,d)) =
            fmap (fmap (,d)) <$> ctx' ((a <$) <$> input)
instance (Monad m) => Arr.ArrowApply (AppContext m) where
    app = 3--hole
--arr :: (Arr.Arrow a) => (b -> c) -> a b c
rev x f = f x
--arr x = Arr.arr x
--(<<<) a b = a Arr.<<< b 
--(>>>) a b = a Arr.>>> b 
gcurry :: forall a b c d. (Arr.Arrow a) => a (b,c) d -> a b (a c d)
gcurry =  rev (Arr.arr (Arr.arr . (,))) . (Arr.<<<) . Arr.arr . (Arr.<<<)

guncurry ::  forall a b c d. (Arr.Arrow a, Arr.ArrowApply a) => a b (a c d) -> a (b,c) d
guncurry = (Arr.app Arr.<<<) . Arr.first
-- hole = undefined
-}