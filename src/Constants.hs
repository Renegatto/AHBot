module Constants
    (   token,
        bot_prefix,
        chatchannelId,
        guildId,
    ) where
import Data.Text
import Data.Word(Word64(..))
import Discord.Internal.Types.Prelude as Discord

token = pack "Njk1NjIwMzc5ODM3ODU3ODQy.Xoo40w._MtnfvNVQ76okVVp0pxWoxhUgmM"
bot_prefix = pack "gay!"

chatchannelId = Snowflake 700363545480659015
guildId       = Snowflake 693476909677412363