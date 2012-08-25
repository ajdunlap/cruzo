{-# LANGUAGE StandaloneDeriving #-}
module Model where

#include "include/imports.h"
import Prelude
import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Quasi
import Codec.Game.Puz
import Database.Persist.Store

--data Sq = B | E | C Char | Rebus Text deriving (Eq,Read,Show)

newtype GridV = GridV { unGridV :: [Square] } deriving (Eq, Read, Show)

instance PersistField GridV where
  toPersistValue (GridV sqs) = PersistText (T.pack $ map go sqs)
    where
      go Black = '.'
      go (Letter Nothing _) = ' '
      go (Letter (Just c) _) = c
  fromPersistValue (PersistText t) = Right . GridV $ map go (T.unpack t)
    where
      go '.' = Black
      go ' ' = Letter Nothing Plain
      go c = Letter (Just c) Plain
  fromPersistValue _ = Left "I only want text"
  sqlType _ = SqlString

textToGridV = GridV . map go . T.unpack
  where go ' ' = Letter Nothing Plain
        go '.' = Black
        go c = Letter (Just c) Plain

gridVToText = T.pack . map go . unGridV
  where go (Letter (Just c) Plain) = c
        go (Letter Nothing Plain) = ' '
        go Black = '.'
        go _ = '#'

{-deriving instance Read Puzzle
deriving instance Read Dir-}
deriving instance Read Square
deriving instance Read Style
$(derivePersistField "Square")

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
