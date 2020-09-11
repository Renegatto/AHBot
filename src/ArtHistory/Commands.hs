
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

type CommandHandler a = Subscription -> a -> DiscordHandler [Event]

hole = undefined

eventAction :: Subscription -> Event -> DiscordHandler ()
eventAction (Subscription _ channel) =
    void . restCall
    . RChan.CreateMessage channel
    . T.pack . show . MessageContent

nextQuiz :: CommandHandler QuizConfig
nextQuiz subscription cfg =
    handleEvents (eventAction subscription) 
    <=< ReaderT . const
    . toIO . fmap (Domain.nextQuiz cfg ) 
    $ withRandomQuizSet cfg

newQuizSeries :: CommandHandler QuizConfig
newQuizSeries subscription cfg =
    handleEvents (eventAction subscription) 
    <=< ReaderT . const
    . toIO . fmap (Domain.newQuizSeries cfg )
    $ withRandomQuizSet cfg

solveQuiz :: CommandHandler (Variant,Quiz)
solveQuiz subscription =
    handleEvents (eventAction subscription)
    . uncurry Domain.solveQuiz

endQuiz :: CommandHandler ()
endQuiz subscription = hole
    -- handleEvents (eventAction discord_channel)
    -- . Domain.endQuiz
    -- <$> 
cmd dc (NextQuiz cfg)      = nextQuiz dc cfg
cmd dc (NewQuizSeries cfg) = newQuizSeries dc cfg

handleEvents :: (Event -> DiscordHandler ()) -> [Event] -> DiscordHandler [Event]
handleEvents action = ($>) =<< mapM_ action

withRandomQuizSet :: QuizConfig -> Compose IO Maybe (Artwork,[Artwork])
withRandomQuizSet = Res.randomQuizSet . cfgTotalVariants

toIO :: Compose IO Maybe [a] -> IO [a]
toIO = fmap (join . maybeToList) . getCompose



    