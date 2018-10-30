#!/usr/bin/env stack
{- stack --resolver lts-12.9 script
   --package shake
   --package aeson
   --package req
   --package data-default-class
   --package text
   --package bytestring
   --package lens
   --package lens-aeson
   --package http-types
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

import Development.Shake
import Development.Shake.FilePath
import Data.Aeson
import Data.Default.Class
import Network.HTTP.Req
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import Control.Lens hiding ((.=))
import Data.Aeson.Lens
import Control.Monad (forM_)
import Network.HTTP.Types (urlDecode)

version = "v0.0.1"
tokenName = "GITHUB_RELEASE_TOKEN"
endpoint = "api.github.com"
owner = "syocy"
repo = "haskell-day-syocy"
pdfs = ["slides/dhall.pdf", "slides/parallel-and-concurrent.pdf"]

main = shakeArgs shakeOptions $ do
  want pdfs
  "slides/*.pdf" %> \out -> do
    let texFile = out -<.> "tex"
    need [texFile]
    command_ [Cwd "slides"] "llmk" []
  phony "nop" $ do
    putNormal "nop"
  phony "once" $ do
    command_ [Cwd "slides"] "llmk" []
  phony "github-release" $ do
    need pdfs
    need ["once"]
    tokenMaybe <- getEnv tokenName
    case tokenMaybe of
      Nothing -> fail "no token found"
      Just token_ -> do
        let token = T.encodeUtf8 $ T.pack token_
        r <- liftIO $ createRelease token
        liftIO $ forM_ pdfs $ \pdf -> do
          fileContents <- LBS.readFile pdf
          let fileName = dropDirectory1 pdf
          uploadReleaseFile token r (T.pack fileName) fileContents
        _ <- liftIO $ finishRelease token r
        return ()


data Release a = Release
  { rUrl :: a
  , rHtmlUrl :: a
  , rUploadUrl :: a
  , rId :: Int
  } deriving (Show)

createRelease :: B.ByteString -> IO (Release T.Text)
createRelease token = runReq def $ do
  liftIO $ print "createRelease"
  let t = id :: T.Text -> T.Text
  let payload = object
        [ "tag_name" .= t version
        , "target_commitish" .= t "master"
        , "name" .= t version
        , "body" .= t "desc"
        , "draft" .= True
        , "prerelease" .= True
        ]
  r <- req POST
    (https endpoint /: "repos" /: owner /: repo /: "releases")
    (ReqBodyJson payload)
    jsonResponse
    (header "Authorization" ("token " <> token)
     <> header "User-Agent" "req/1.1.0")
  let res = responseBody r :: Value
  let releaseMaybe = do
                      rUrl1 <- res ^? key "url" . _String
                      rHtmlUrl1 <- res ^? key "html_url" . _String
                      rUploadUrl1 <- res ^? key "upload_url" . _String
                      rId1 <- res ^? key "id" . _Integer
                      return $ Release rUrl1 rHtmlUrl1 rUploadUrl1 (fromInteger rId1)
  case releaseMaybe of
    Just r -> return r
    Nothing -> fail "failed to parse"

uploadReleaseFile :: B.ByteString -> Release T.Text -> T.Text -> LBS.ByteString -> IO ()
uploadReleaseFile token Release{..} fileName fileContents = runReq def $ do
  liftIO $ print "uploadReleaseFile"
  let urlMaybe = parseUrlHttps $ T.encodeUtf8 $ T.replace "{" "" rUploadUrl
  case urlMaybe of
    Nothing -> fail "failed to parse upload url"
    Just (url, opts) -> do
      r <- req POST
        url
        (ReqBodyBs $ LBS.toStrict fileContents)
        jsonResponse
        ( ("name" =: fileName)
          <> header "Content-Type" "application/pdf"
          <> header "Authorization" ("token " <> token)
          <> header "User-Agent" "req/1.1.0")
      liftIO $ print $ (responseBody r :: Value)
      return ()

finishRelease :: B.ByteString -> Release T.Text -> IO ()
finishRelease token Release{..} = runReq def $ do
  liftIO $ print "finishRelease"
  let payload = object
        [ "draft" .= False
        , "prerelease" .= True
        ]
  r <- req PATCH
    (https endpoint /: "repos" /: owner /: repo /: "releases" /: (T.pack $ show rId))
    (ReqBodyJson payload)
    jsonResponse
    ( header "Authorization" ("token " <> token)
      <> header "User-Agent" "req/1.1.0")
  let res = responseBody r :: Value
  liftIO $ print res
  return ()