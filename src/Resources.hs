{-# LANGUAGE FlexibleInstances, GADTs, InstanceSigs #-}
module Resources ( artworks
                 , randomQuizSet
                 -- , randomImage
                 -- , images
                 , randomQuizSet'
                 ) where
import Data.Maybe (fromMaybe)
--import Data.Functor(($>))
import Control.Monad(mzero,mplus,MonadPlus(..),join,liftM2)
import Control.Applicative(Alternative(..))

import ArtHistory.Types as Art
import Types.Common
import System.Random(randomRIO,getStdGen,RandomGen(..),randoms)
import Data.Functor.Compose
import Data.List(sortOn)
import Data.Monoid
import Data.Functor.Sum

import qualified Constants as Const (artworks_store)
import Data.Aeson (decodeFileStrict)
import Schemes


hole = 7 :: Int

randomArtworks :: Int -> IOMaybe [Art.Artwork]
randomArtworks n = Compose $ sequence . randomSample n =<< artworks

randomQuizSet :: Int -> IOMaybe (Art.Artwork,[Art.Artwork])
randomQuizSet n = 
  (flip (fmap . flip (,)) =<< randomElem') =<< randomArtworks n

randomQuizSet' :: Int -> IO (Maybe (Art.Artwork,[Art.Artwork]))
randomQuizSet' n = getCompose $ randomQuizSet n

artworks :: IO [Art.Artwork]
artworks = fromMaybe [] <$> decodeFileStrict Const.artworks_store

-- =============
{-
randomImage :: IOMaybe Image
randomImage = randomFrom images

images :: IO [Image]
images = hole
-}
-- =============

randomFrom :: IO [a] -> IOMaybe a
randomFrom xs = Compose $ sequenceA . randomElem =<< xs

randomElem' :: [a] -> IOMaybe a
randomElem' = Compose . sequence . randomElem

randomElem :: [a] -> Maybe (IO a)
randomElem [] = Nothing
randomElem xs = Just $ (xs !!) <$> randomRIO (0,length xs - 1)

shuffleByGen :: RandomGen gen => gen -> [a] -> [a]
shuffleByGen gen = map snd . sortOn fst . zip (randoms gen :: [Int])

shuffle :: [a] -> IO [a]
shuffle = liftM2 shuffleByGen getStdGen . pure

randomSample :: Int -> [a] -> Maybe (IO [a])
randomSample n xs
    |length xs < n = Nothing
    |otherwise = Just $ take n <$> shuffle xs

type IOMaybe = Compose IO Maybe

instance (Traversable m, Alternative m, Monad m) => Monad (Compose IO m) where
  return x = Compose $ pure $ pure x
  (>>=) :: forall a b. Compose IO m a -> (a -> Compose IO m b) -> Compose IO m b
  (Compose x) >>= f = (Compose :: IO (m b) -> Compose IO m b)
        $ fmap join . sequence . h . f'
        =<< (x :: IO (m a))
        where
        f' = fmap f :: m a -> m (Compose IO m b)
        h = fmap $ (empty <|>) . getCompose :: m (Compose IO m b) -> m (IO (m b))
instance MonadPlus IOMaybe where
  mzero = Compose $ pure Nothing