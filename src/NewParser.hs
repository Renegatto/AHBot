import Text.Parsec
import qualified Constants
import Data.Functor.Identity
import Data.Text as T
import Data.Bool (bool)
{-
bot_prefix :: Parsec s u String
bot_prefix = token 
                    show 
                    undefined -- fst 
                    undefined -- (\(_,x) -> if x == "foo" then Just True else Nothing)
-}