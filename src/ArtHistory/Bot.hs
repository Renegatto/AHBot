module ArtHistory.Bot ( createEnv
                      , artHistoryCommand
                      , artHistoryEvent
                      , message2sub ) where

import qualified Data.Text            as T
import           Control.Monad.Reader          (lift)
import           Control.Monad                 (liftM4,void)

import           Control.Concurrent.Chan       (newChan,writeChan,writeList2Chan)
import           Data.IORef                    (newIORef,modifyIORef,readIORef)

import           Discord                                      (DiscordHandler)
import qualified Discord.Internal.Rest         as DiscordRest
import qualified Discord.Internal.Rest.Channel as RChann

import           Tools.Combinators                  (addToIORef)
import           Types.Common         hiding        (Message(..))
import qualified ArtHistory.Commands  as AHCommands (handle)

import qualified ArtHistory.Types     as AH
import qualified ArtHistory.Events    as EH         (handleEvent)
import qualified ArtHistory.Messages  as Messages   (Debug(..))

import qualified ArtHistory.Languages.Interpreters as Interpreter (AppL,evalAppL) 
import qualified ArtHistory.Languages.Language     as AppL        (pushEvents)

type AHApp = AppData (Sub AH.Event) (Sub AH.Command)

createEnv :: IO AHApp
createEnv = liftM4 AppData (newIORef []) newChan (newIORef []) newChan

artHistoryEvent :: AHApp -> Sub AH.Event -> DiscordHandler ()
artHistoryEvent appdata =  lift . actions . sequence . EH.handleEvent
  where 
  actions :: [Sub AH.Command] -> IO ()
  actions commands = do
      writeList2Chan (commandHub appdata)     commands
      addToIORef     (commandHistory appdata) commands

artHistoryCommand :: AHApp -> Sub AH.Command -> DiscordHandler ()
artHistoryCommand appdata (Sub sub command) = do
  result <- Interpreter.evalAppL appdata sub $ AHCommands.handle command
  case result of
    Right x  -> pure ()
    Left err -> void 
                $ Interpreter.evalAppL appdata sub 
                $ AppL.pushEvents [AH.DomainError err]

user2sub :: DiscordRest.User -> Subscriber
user2sub = Subscriber . DiscordRest.userId

message2sub :: DiscordRest.Message -> Subscription
message2sub = Subscription . user2sub . DiscordRest.messageAuthor <*> DiscordRest.messageChannel


