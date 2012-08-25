{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.New where

#include "../include/imports.h"
import Yesod.Auth
import Database.Persist.Store
import Codec.Game.Puz
import Data.Array
import Import

--newGrid :: Int -> Int -> Handler 
newGrid rs cs t a u = runDB $ Import.insert (Puz u rs cs t a (GridV $ replicate (rs*cs) (Letter Nothing Plain)) Nothing)

getNewR = do
  (form,enctype) <- generateFormPost rcform
  defaultLayout $ do
    setTitle "New Crossword"
    $(widgetFile "new")
  
rcform = renderBootstrap $ (,) <$> areq intField "Rows" Nothing <*> areq intField "Columns" Nothing

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
postNewR :: Handler RepJson
postNewR = do
  u <- requireAuthId
  ((result,widget),enctype) <- runFormPost rcform
  case result of
    FormSuccess (r,c) -> do
      Key (PersistInt64 k) <- newGrid r c "" "" u -- runDB $ Import.insert (Puz (u::UserId) 15 15 (replicate 225 (Letter Nothing Plain)))
      redirect (GridR (fromIntegral k))
      -- jsonToRepJson (object ["id" .= show k])
