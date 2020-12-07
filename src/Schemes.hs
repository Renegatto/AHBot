{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module Schemes where
import Data.Aeson
import GHC.Generics(Generic)

import ArtHistory.Types
import Types.Common(Image(..))

deriving instance Generic Artwork
deriving instance Generic Image

instance ToJSON Image
instance ToJSON Artwork
instance FromJSON Image
instance FromJSON Artwork

deriving instance Generic Art
deriving instance Generic QuizConfig

instance ToJSON Art
instance ToJSON QuizConfig
instance FromJSON Art
instance FromJSON QuizConfig