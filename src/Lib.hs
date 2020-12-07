module Lib where
import ArtHistory.Types
import Schemes
import Types.Common (Image(..))
import Data.Aeson
import Constants (artworks_store)
import Control.Monad (liftM2,forever)
import Control.Concurrent.MVar(putMVar,newMVar,readMVar,modifyMVar_,MVar(..))

import Control.Concurrent.Chan (writeChan,newChan,getChanContents)
{-



sampleArtwork :: Artwork
sampleArtwork = 
    Artwork { artworkAuthor = "Pablo Micorezzi"
            , artworkYear   = "1939 - 228"
            , artworkName   = "Рождение Венеры"
            , artworkImage  = Image "http://example-image-domain.dom/veneras-birthday.jpeg"
            , artworkArt    = Art "Средней Азии"}

storeSample :: IO ()
storeSample = encodeFile artworks_store 
                [sampleArtwork
                ,sampleArtwork {artworkName="Abraham Linkoln"}]

someFunc :: IO ()
someFunc = addStuffLoop
-}