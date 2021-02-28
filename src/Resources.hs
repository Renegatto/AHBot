{-# LANGUAGE FlexibleInstances, GADTs, InstanceSigs, TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Resources ( artworks
                 --, randomQuizSet
                 -- , randomImage
                 -- , images
                 , randomQuizSet
                 ) where
import Data.Maybe (fromMaybe)
--import Data.Functor(($>))
import Control.Monad(mzero,mplus,MonadPlus(..),join,liftM2)
import Control.Applicative(Alternative(..))

import ArtHistory.Types as Art ( Artwork, artworkName, artworkAuthor, artworkYear )
import Types.Common ()
import System.Random(randomRIO,getStdGen,RandomGen(..),randoms,Random(..))
import Data.Functor.Compose ( Compose(..) )
import Data.List(sortOn, intercalate)
import Data.Monoid ()
import Data.Functor.Sum ()

import qualified Data.List.NonEmpty as NE (NonEmpty(..),zip,sortWith, cons, (!!), fromList, toList, nonEmpty)
import qualified Constants as Const (artworks_store)
import Data.Aeson (decodeFileStrict)
import Schemes ()
import Control.Arrow((&&&),(***),first, Arrow (second))
import Optics(makeLenses, view, preview, _Right, (%), each, (^?),_head,_Left,_1,_Just,_2)

hole :: Int
hole = 7 :: Int
newtype Shuffled a = Shuffled {_shuffled :: a}
makeLenses ''Shuffled

data QuizSetGeneratingError 
    = NotEnoughArtworksInStorage 
    | ArtworksStorageIsEmpty deriving (Show,Eq)

res :: IO [Either QuizSetGeneratingError (Artwork, NE Artwork)]
res = traverse (const $ randomQuizSet 3) [0..40] :: IO [Either QuizSetGeneratingError (Art.Artwork, NE Art.Artwork)]

qeee = fmap (^? _Right % _1 % artworkYear) <$> res
bar = traverse print <$> res
-- >>> res = traverse (const $ randomQuizSet 3) [0..10] :: IO [Either QuizSetGeneratingError (Art.Artwork, NE Art.Artwork)]
-- >>> traverse print <$> res
-- No instance for (Show (IO [()])) 


-- >>> fmap length . preview (_Right % _2) <$> randomQuizSet 2
-- Just 2

--valid one
randomQuizSet :: Int -> IO (Either QuizSetGeneratingError (Art.Artwork, NE Art.Artwork))
randomQuizSet n = do
    arts <- artworks
    shuffle_arts <- randomIO :: IO ([Art.Artwork] -> Shuffled [Art.Artwork])
    if length arts < n
    then pure $ Left NotEnoughArtworksInStorage
    else do
        get_quiz_set <- randomIO :: IO (NE Art.Artwork -> (Art.Artwork,NE Art.Artwork))
        pure $ get_quiz_set <$> right_size (shuffle_arts arts)
    where
    right_size = 
        maybe (Left ArtworksStorageIsEmpty) Right
        . NE.nonEmpty . take n . view shuffled  

-- need to get replaced by randomQuizSet, because (Art,[Art]) isnt valid state
-- because in (a,[as]) 'a' is elem of 'as', that means that 'as' can be empty
randomQuizSet' :: Int -> IO (Maybe (Art.Artwork,[Art.Artwork])) 
randomQuizSet' n = do
    get_quiz_set <- randomIO :: IO (NE Art.Artwork -> (Art.Artwork,NE Art.Artwork))
    arts <- NE.nonEmpty <$> artworks
    pure $ second NE.toList . get_quiz_set <$> arts

artworks :: IO [Art.Artwork]
artworks = fromMaybe [] <$> decodeFileStrict Const.artworks_store

cyclicByIndex :: Int -> NE.NonEmpty a -> a
cyclicByIndex i xs = xs NE.!! mod i (length xs)

type Gen b a = b -> a
type QuizSet = (Art.Artwork,NE Art.Artwork)
type NE = NE.NonEmpty


instance Random ([a] -> Shuffled [a]) where
  random gen = (Shuffled . shuff, g2)
    where
    (indicies :: [Int],g2) = first randoms $ split gen
    shuff = fmap snd . sortOn fst . zip indicies

instance Random (NE a -> Shuffled (NE a)) where
  random gen = (Shuffled . shuff, g2)
    where
    ((el,elems),g2) = first (second randoms . random) $ split gen
    shuff = fmap snd . NE.sortWith fst . NE.zip (el NE.:| elems :: NE Int)

instance Random (NE a -> a) where
  random gen = (cyclicByIndex i,g1)
    where (i,g1) = random gen

instance Random (NE a -> (a,NE a)) where
  random gen = (elem . view shuffled . shuffd &&& id,gen2)
    where 
    ( shuffd :: NE a -> Shuffled (NE a),
     (elem :: NE a -> a, gen2)        ) = second random $ random gen
