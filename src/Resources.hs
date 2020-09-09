module Resources ( randomArtwork
                 , artworks
                 , randomImage
                 , images
                 ) where
import ArtHistory.Types as Art
import Types.Common
import System.Random(randomRIO,getStdGen,RandomGen(..),randoms)
import Data.Functor.Compose
import Data.List(sortOn)
import Control.Monad(liftM2)
randomArtwork :: IOMaybe Art.Artwork
randomArtwork = randomFrom artworks

artworks :: IO [Art.Artwork]
artworks = undefined

-- =============

randomImage :: IOMaybe Image
randomImage = randomFrom images

images :: IO [Image]
images = undefined

-- =============

randomFrom :: IO [a] -> IOMaybe a
randomFrom xs = Compose $ sequenceA . randomElem =<< xs

randomElem :: [a] -> Maybe (IO a)
randomElem [] = Nothing
randomElem xs = Just $ (xs !!) <$> randomRIO (0,length xs - 1)

shuffleByGen :: RandomGen gen => gen -> [a] -> [a]
shuffleByGen gen = map snd . sortOn fst . zip [randoms gen :: [Int]]

shuffle :: [a] -> IO [a]
shuffle = liftM2 shuffleByGen getStdGen . pure

type IOMaybe = Compose IO Maybe