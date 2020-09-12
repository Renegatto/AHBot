
module ArtHistory.Commands where
import ArtHistory.Types
import ArtHistory.Messages(MessageContent(..))
import Types.Common
import qualified ArtHistory.Domain as Domain
import qualified Resources as Res (randomQuizSet)

import Data.Functor.Compose
import Data.Functor(($>))
import Control.Monad(void,mapM_,join,(<=<))
import Data.Maybe(fromMaybe,maybeToList)
import Data.List(find)
import Data.Foldable(foldMap)

import Discord (DiscordHandler,restCall)
import Discord.Internal.Rest.Channel as RChan

import Control.Monad.Reader(ReaderT(..))
import qualified Data.Text as T

type CommandProcessor a = a -> IO [Event]
type CommandHandler = Command -> IO [Event]

nextQuiz :: CommandProcessor QuizConfig
nextQuiz cfg =
    toIO . fmap (Domain.nextQuiz cfg ) 
    $ withRandomQuizSet cfg

newQuizSeries :: CommandProcessor QuizConfig
newQuizSeries cfg =
    toIO . fmap (Domain.newQuizSeries cfg )
    $ withRandomQuizSet cfg

solveQuiz :: CommandProcessor (Variant,Quiz)
solveQuiz = pure . uncurry Domain.solveQuiz

endQuiz :: CommandProcessor ()
endQuiz = undefined
 
handle :: CommandHandler
handle (NextQuiz cfg) = nextQuiz cfg
handle (NewQuizSeries cfg) = newQuizSeries cfg

--handleEvents :: (Event -> DiscordHandler ()) -> [Event] -> DiscordHandler [Event]
--handleEvents action = ($>) =<< mapM_ action

withRandomQuizSet :: QuizConfig -> Compose IO Maybe (Artwork,[Artwork])
withRandomQuizSet = Res.randomQuizSet . cfgTotalVariants

toIO :: Compose IO Maybe [a] -> IO [a]
toIO = fmap (join . maybeToList) . getCompose



    