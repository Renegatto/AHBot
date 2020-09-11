module Constants
    (   token,
        bot_prefix,
        chatchannelId,
        guildId
    ) where
import Data.Text
import Data.Word(Word64(..))
import qualified Discord.Internal.Types as Discord

token = pack "Njk1NjIwMzc5ODM3ODU3ODQy.Xoo40w._MtnfvNVQ76okVVp0pxWoxhUgmM"
bot_prefix = pack "ь"

chatchannelId = Discord.Snowflake 693476910587445311
guildId       = Discord.Snowflake 693476909677412363


--ART HISTORY HELPER
