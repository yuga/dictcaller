{-# LANGUAGE OverloadedStrings #-}

module Server (
    run
) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A 
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (IsString)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL 
import qualified Dictionary
import qualified ItemTable
import Network.HTTP.Types (status404)
import Network.Wai.Middleware.Static (static)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty (ActionM, json, get, middleware, params, post, raise, rescue, scotty, status, text)
import Web.Scotty.Trans (ActionT)

run :: Int -> IO ()
run port = scotty port $ do
    middleware static
    middleware logStdoutDev

    -- search word
    get "/search" $ rescueJSON $
        search

    post "/search" $ rescueJSON $
        search

    get "/say" $ rescueJSON $ do
        undefined

rescueJSON :: ActionM () -> ActionM ()
rescueJSON action = action `rescue` \msg -> do
    liftIO $ print msg
    json False

data SearchParam = NoParam
                 | JsonParam BL.ByteString
                 | TextParam TL.Text

parseParams :: (Eq a, IsString a) => [(a, TL.Text)] -> SearchParam
parseParams ps =
    let q = filter (\(k,_) -> k == "q") ps in
    if length q == 0
      then NoParam
      else if any (\(k,v) -> k == "m" && v == "json") ps
             then JsonParam (TL.encodeUtf8 . snd . head $ q)
             else TextParam (snd . head $ q)

search :: ActionT IO ()
search = do
    ps <- params
    case parseParams ps of
      NoParam -> raise $ "Invalid param: " <> (TL.pack . show $ ps)
      JsonParam q -> do
          -- liftIO $ print "JsonParam" >> print (A.decode q :: Maybe [TL.Text])
          items <- liftIO $ Dictionary.search . fromMaybe [] $ (A.decode q)
          json $ foldl toMap Map.empty items 
      TextParam q -> do
          -- liftIO $ print "TextParam" >> print q
          items <- liftIO $ Dictionary.search [q]
          if length items <= 0
              then status status404 
              else text (ItemTable.trans . head $ items)
  where
    toMap m item = Map.insert key value m
      where
        key   = ItemTable.word  item
        value = Map.fromList [("trans",trans),("pron",pron)] :: Map.Map TL.Text TL.Text
        trans = TL.replace "\\" "\n" (ItemTable.trans item)
        pron  = TL.replace "\\" "\n" (ItemTable.pron  item)
