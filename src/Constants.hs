module Constants
    (   token,
        bot_prefix,
        chatchannelId,
        guildId,
        artworks_store,
        bot_id,
        ah_bot_prefix,
    ) where
import Data.Text
import Data.Word(Word64(..))
import qualified Discord.Internal.Types as Discord

token = pack "Njk1NjIwMzc5ODM3ODU3ODQy.Xoc09g.rwvcq-ufLleozmwp_GWJ2rjsazs"
bot_prefix = pack "ь"
ah_bot_prefix = "ah."
artworks_store = "artworks.json"
bot_id = Discord.Snowflake 695620379837857842

chatchannelId = Discord.Snowflake 693476910587445311
guildId       = Discord.Snowflake 693476909677412363


--ART HISTORY HELPER
