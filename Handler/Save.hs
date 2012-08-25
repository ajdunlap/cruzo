{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Save where

#include "../include/imports.h"
import Import
import Data.Maybe
import Control.Arrow
import Control.Monad
import Control.Monad.Maybe
import Database.Persist.Store
import Data.Either
import qualified Data.Text.Read as TR
import Yesod.Auth

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
postSaveR :: Handler RepJson
postSaveR = do
  u <- requireAuthId
   -- wr <- waiRequest
   -- bss <- lift $ lazyConsume $ requestBody wr
  bss <- fst <$> runRequestBody
  k <- runMaybeT $ do
    i <- mr (fmap (Key . PersistInt64 . fromIntegral) . decimalMay =<< lookup "puzzleId" bss)
    -- rs <- mt $ decimalMay =<< lookup "rows" bss
    -- cs <- mt $ decimalMay =<< lookup "cols" bss
    gr <- fmap textToGridV . mr $ lookup "grid" bss
    lsgr <- mr $ lookup "lastSavedGrid" bss
    clientId <- mr $ lookup "clientId" bss
    (realLsgr,lastSavedClientId) <- fmap (puzGrid &&& puzLastSavedClientId) . MaybeT . runDB $ get i
    -- guard (textToGridV lsgr == realLsgr)
    return (i,gr,lsgr,realLsgr,clientId,lastSavedClientId)
  case k of
    Just (i,gr,lsgr,realLsgr,clientId,lastSavedClientId) -> do
      if (isNothing lastSavedClientId || clientId == fromJust lastSavedClientId || textToGridV lsgr == realLsgr)
        then do runDB $ update i [PuzGrid =. gr, PuzLastSavedClientId =. Just clientId]
                jsonToRepJson (object ["ok" .= True, "realLsgr" .= gridVToText realLsgr])
        else jsonToRepJson (object ["ok" .= False, "realLsgr" .= gridVToText realLsgr])
    Nothing -> jsonToRepJson (object ["ok" .= False])

{-
  case (,,,,) <$> lookup "puzzleId" bss <*> (decimalMay =<< lookup "rows" bss) <*> (decimalMay =<< lookup "cols" bss) <*> lookup "grid" bss <*> lookup "lastSavedGrid" bss of
    Just (i,rs,cs,gr,lsg) -> let puz = Puz u rs cs (textToGridV gr)
                             in do realLastSavedGrid <- runDB $ get (read . T.unpack $ i)
                                   return ()
  liftIO $ print $ bss
  -- let requestBody = L.fromChunks bss
  return $ RepJson ""
  -}
    where decimalMay :: Text -> Maybe Int
          decimalMay = fmap fst . either (const Nothing) Just . TR.decimal
          mr = MaybeT . return

{-
postGridR :: Handler RepHtml
postGridR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")
        -}
