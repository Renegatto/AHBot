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
import qualified ArtHistory.Languages as AHLangs    (AppL,evalAppL,pushEvents) 
import qualified ArtHistory.Types     as AH
import qualified ArtHistory.Events    as EH         (handleEvent)
import qualified ArtHistory.Messages  as Messages   (Debug(..))

type AHApp = AppData (Sub AH.Event) (Sub AH.Command)

createEnv :: IO AHApp
createEnv = liftM4 AppData (newIORef []) newChan (newIORef []) newChan

artHistoryEvent :: AHApp -> Sub AH.Event -> DiscordHandler ()
artHistoryEvent appdata event = EH.handleEvent event >>= lift . actions . sequence
  where 
  actions :: [Sub AH.Command] -> IO ()
  actions commands = do
      --print "commands added"
      writeList2Chan (commandHub appdata)     commands
      addToIORef     (commandHistory appdata) commands
      --print . map (Messages.Debug . subscriptionStored) 
      --      =<< readIORef (commandHistory appdata)

artHistoryCommand :: AHApp -> Sub AH.Command -> DiscordHandler ()
artHistoryCommand appdata (Sub sub command) = do
  result <- AHLangs.evalAppL appdata sub $ AHCommands.handle command
  case result of
    Right x  -> pure ()
    Left err -> void 
                $ AHLangs.evalAppL appdata sub 
                $ AHLangs.pushEvents [AH.DomainError err]

user2sub :: DiscordRest.User -> Subscriber
user2sub = Subscriber . DiscordRest.userId

message2sub :: DiscordRest.Message -> Subscription
message2sub = Subscription . user2sub . DiscordRest.messageAuthor <*> DiscordRest.messageChannel


