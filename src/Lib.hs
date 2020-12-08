module Lib where
import ArtHistory.Types
import Schemes
import Types.Common (Image(..))
import Data.Aeson
import Constants (artworks_store)
import Control.Monad (liftM2,forever)
import Control.Concurrent.MVar(putMVar,newMVar,readMVar,modifyMVar_,MVar(..))

import Control.Concurrent.Chan (writeChan,newChan,getChanContents)




sampleArtwork :: Artwork
sampleArtwork = 
    Artwork { _artworkAuthor = "Pablo Micorezzi"
            , _artworkYear   = "1939 - 228"
            , _artworkName   = "Рождение Венеры"
            , _artworkImage  = Image "http://example-image-domain.dom/veneras-birthday.jpeg"
            , _artworkArt    = Art "Средней Азии"}

storeSample :: IO ()
storeSample = encodeFile artworks_store 
                [sampleArtwork
                ,sampleArtwork {_artworkName="Abraham Linkoln"}]

--someFunc :: IO ()
--someFunc = addStuffLoop
