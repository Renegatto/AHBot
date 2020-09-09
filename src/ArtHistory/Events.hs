module ArtHistory.Events where
import ArtHistory.Types
import Resources (randomArtwork,artworks)

nextQuiz cfg = 3

cmd (NextQuiz cfg) = nextQuiz cfg
