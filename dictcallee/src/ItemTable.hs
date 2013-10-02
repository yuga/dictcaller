{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ItemTable where

import Data.Text.Lazy (Text)
import Database.Record.TH (derivingShow)
import SQLite3DataSource (defineTable)

$(defineTable [("text",[t|Text|])] "main" "ItemTable" [derivingShow])
