{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Grid where

import Import
import Database.Persist.Store
import Control.Monad
import Yesod.Auth
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getGridR :: Int -> Handler RepHtml
getGridR i = do
  {-
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
        -}
    u <- requireAuthId
    Puz u' r c t a gr _ <- runDB $ get404 (Key $ PersistInt64 $ fromIntegral i)
    when (u /= u') notFound
    clientId <- liftIO UUID.nextRandom
    liftIO $ print clientId
    liftIO $ print "foobar"
    -- let _ = clientId :: UUID.UUID
    {-
    gr <- case gr' of
            Nothing -> notFound
            Just k -> return k
            -}
      
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        let rows = 15 :: Int
            cols = 15 :: Int
        $(widgetFile "grid")

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

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
