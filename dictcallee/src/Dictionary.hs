{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}

module Dictionary (
    search 
) where

import qualified Data.Text.Lazy as TL
import Database.HDBC (handleSqlError, SqlValue)
import Database.HDBC.Record.Query (fetchAll', prepare)
import Database.HDBC.Record.Statement (bindTo, execute)
import Database.HDBC.Session (withConnectionIO)
import Database.Record (FromSql, ToSql)
import Database.Relational.Query (Relation, placeholder, query, relation',
    relationalQuery, wheres, (!), (.=.))
import qualified ItemTable
import ItemTable (ItemTable, itemTable)
import SQLite3DataSource (connect) 

search :: [TL.Text] -> IO [ItemTable] 
search qs = runQueries findByWord qs

runQueries :: (FromSql SqlValue r, ToSql SqlValue p, Show p, Show r) => Relation p r -> [p] -> IO [r]
runQueries rel xs = handleSqlError . withConnectionIO connect $ \conn -> do
    -- putStrLn $ "SQL: " ++ show rel
    pq <- prepare conn (relationalQuery rel)
    rss <- mapM (f pq) xs
    return $ concat rss
  where
    f pq x = execute (x `bindTo` pq) >>= \bs -> fetchAll' bs

findByWord :: Relation TL.Text ItemTable
findByWord =
    relation' $
    [ (wordP, i)
    | i <- query itemTable
    , (wordP, ()) <- placeholder (\ph -> wheres $ i ! ItemTable.word' .=. ph)
    ]
