module Unison.Sqlite.Utils (likeEscape) where

import Data.Text (Text)
import Data.Text qualified as Text

-- | Escape special characters for "LIKE" matches.
--
-- Prepared statements prevent sql injection, but it's still possible some user
-- may be able to craft a query using a fake "hash" that would let them see more than they
-- ought to.
--
-- You still need to provide the escape char in the sql query, E.g.
--
-- @@
--   SELECT * FROM table
--     WHERE txt LIKE ? ESCAPE '\'
-- @@
--
-- >>> likeEscape '\\' "Nat.%"
-- "Nat.\%"
likeEscape :: Char -> Text -> Text
likeEscape '%' _ = error "Can't use % or _ as escape characters"
likeEscape '_' _ = error "Can't use % or _ as escape characters"
likeEscape escapeChar pat =
  flip Text.concatMap pat \case
    '%' -> Text.pack [escapeChar, '%']
    '_' -> Text.pack [escapeChar, '_']
    c
      | c == escapeChar -> Text.pack [escapeChar, escapeChar]
      | otherwise -> Text.singleton c
