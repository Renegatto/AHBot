{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module ArtHistory.Commands where
import ArtHistory.Types
import ArtHistory.Messages(MessageContent(..))
import Types.Common hiding (Error(..))
import qualified ArtHistory.Domain as Domain
import qualified Resources as Res (randomQuizSet)
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor(($>))
import Control.Monad(void,mapM_,join,(<=<))
import Data.Maybe(fromMaybe,maybeToList)
import Data.List(find)
import Data.Foldable(foldMap)
import Data.Function((&))

import Discord (DiscordHandler,restCall)
import Discord.Internal.Rest.Channel as RChan

import Control.Monad.Except(ExceptT(..),Except,runExceptT)
import Control.Monad.Reader(ReaderT(..))
import qualified Data.Text as T

import Control.Monad.IO.Class

resultFromExcept r =
    result <$> runExceptT r where
        result (Left x) = Fail x
        result (Right x) = Ok x

nextQuiz :: AppContext () IO [Event]
nextQuiz = readContext' $

    \(app, Sub sub _) ->
        resultFromExcept $ next_quiz sub app =<< getConfig sub app
    where 
    
    next_quiz sub app cfg = 
        ExceptT . fmap Right . toIO . fmap (Domain.nextQuiz cfg)
        $ withRandomQuizSet cfg

    getConfig :: Subscription -> App -> ExceptT Error IO QuizConfig
    getConfig = hole

    quiz_not_found = pure . Fail . Error 
        $ "NextQuiz evaluating error: quiz config not found"
        
newQuizSeries :: AppContext QuizConfig IO [Event]
newQuizSeries = readContext' $
    \(app, Sub _ cfg) -> 
        fmap Ok . toIO 
        . fmap (Domain.newQuizSeries cfg )
        $ withRandomQuizSet cfg

solveQuiz :: AppContext Variant IO [Event]
solveQuiz = readContext' $
    \(app, Sub sub variant) -> 
        resultFromExcept
        $ solve app variant =<< find_quiz sub app
    where
    quiz_not_found = pure . Fail . Error 
        $ "solveQuiz evaluating error: quiz not found"
        
    solve app variant =
        pure . Domain.trySolve variant 

    find_quiz :: Subscription -> App -> ExceptT Error IO Quiz
    find_quiz = hole

endQuizSeries :: CommandProcessor ()
endQuizSeries = undefined

type CommandProcessor a = a -> IO [Event]
type CommandHandler = AppContext Command IO [Event]

handle :: CommandHandler
handle =
    readContext $ \(app,command) ->
        \case     
            NextQuiz               -> 
                runContext nextQuiz (cont cfg)  -- IO (Sub (Result [Event]))
            {-NewQuizSeries cfg      -> 
                runContext newQuizSeries (cont cfg) 
            SolveQuiz variant      -> 
                runContext solveQuiz (cont variant) 
            EndQuizSeries          -> 
                runContext endQuizSeries (cont ())
            SendMessage msg        -> 
                pure $ (<$ command) $ Ok [MessageSent msg]-}
        where cont x = (app, x <$ command)

withRandomQuizSet :: QuizConfig -> Compose IO Maybe (Artwork,[Artwork])
withRandomQuizSet = Res.randomQuizSet . cfgTotalVariants

toIO :: Compose IO Maybe [a] -> IO [a]
toIO = fmap (join . maybeToList) . getCompose


hole = undefined
    