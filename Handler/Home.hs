{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Control.Arrow
import Yesod.Auth

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    {-(formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text -}
    k <- fmap (entityKey &&& userIdent.entityVal) <$> maybeAuth
    case k of
      Just (i,u) -> do
        puzzles <- runDB $ selectList [PuzUser ==. i] []
        defaultLayout $ do
          aDomId <- lift newIdent
          setTitle "Welcome To Yesod!"
          $(widgetFile "homepage")
      Nothing -> defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "welcome"
        $(widgetFile "homepage-notloggedin")

{-
postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
    -}
