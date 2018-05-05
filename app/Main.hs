{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import Data.Maybe (fromJust)
import Data.String (fromString)
import qualified Data.ByteString.Lazy as LBS

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Parse
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Method as M
import qualified Network.Wai.Application.Static as Static
import WaiAppStatic.Types (toPieces)

import qualified Data.Aeson as AE

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Aeson.Types (ToJSON,toJSON,object,(.=))

import qualified Data.ByteString as BS
import Data.Char (isDigit)
import Data.Either (partitionEithers)

--import Data.Monoid ((<>))

--import qualified Data.Text.Lazy as LT
--import qualified Data.Text.Lazy.Encoding as LT

--import Data.List (sort)




main :: IO ()
main = do
  host:port:_ <- getArgs
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
    Warp.defaultSettings
    ) $ routerApp

    
routerApp :: Wai.Application
routerApp req respond = case Wai.pathInfo req of
  "exp-hs-http" : _ -> expApp req respond --[TODO] catch exception
  _ -> staticApp  req respond -- static html/js/css files


expApp :: Wai.Application
expApp req respond = case (M.parseMethod (Wai.requestMethod req), Wai.pathInfo req) of
  (Right M.POST, [_, "ep1"])  -> ep1App req respond -- /exp-hs-http/ep1
  (Right M.POST, [_, "ep2"])  -> respond $ responseTxt "ep2" -- /exp-hs-http/ep2
  _ -> staticApp req respond -- static html/js/css files


{--
curl -X POST \
  -d "name=三辻尚栄" \
  -d "zipcode=9110034" \
  http://localhost:8080/exp-hs-http/ep1 | jq .
--}
ep1App :: Wai.Application
ep1App req respond = do
  (ps,_) <- Parse.parseRequestBody Parse.lbsBackEnd req
  case partitionEithers $ fmap (\f->f ps) [validName,validZipcode] of
    ([],ts) -> respond $ responseTxt $ AE.encode Success -- [TODO] execution
    (es,_)  -> respond $ response500 $ AE.encode $ ValidError es
  where
    validName :: [Parse.Param] -> Either ValidErrorMessage (T.Text,T.Text)
    validName ps =
      let
        bkey = "name"
        tkey = T.decodeUtf8 bkey
      in case lookup bkey ps of
        Nothing -> Left $ ValidErrorMessage tkey "名前が指定されていません"
        Just bs -> case T.decodeUtf8 bs of
          ts | T.length ts == 0 -> Left $ ValidErrorMessage tkey "名前が指定されていません"
          ts | T.length ts > 20 -> Left $ ValidErrorMessage tkey "名前は20文字以内で入力してください"
          ts -> Right (tkey, ts)

    validZipcode :: [Parse.Param] -> Either ValidErrorMessage (T.Text,T.Text)
    validZipcode ps =
      let
        bkey = "zipcode"
        tkey = T.decodeUtf8 bkey
      in case lookup bkey ps of
        Nothing -> Left $ ValidErrorMessage tkey "郵便番号が指定されていません"
        Just bs -> case T.decodeUtf8 bs of
          ts | T.length ts /= 7       -> Left $ ValidErrorMessage tkey "郵便番号は7桁で指定してください"
          ts | not (T.all isDigit ts) -> Left $ ValidErrorMessage tkey "郵便番号は半角数字で指定してください"
          ts -> Right (tkey, ts)



staticApp :: Wai.Application
staticApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.defaultWebAppSettings "static"
    indices = fromJust $ toPieces ["main.html"] -- default content



data Success = Success
data ValidError = ValidError [ValidErrorMessage]
data ValidErrorMessage = ValidErrorMessage T.Text T.Text

instance ToJSON Success where
  toJSON (Success) =
    object ["result" .= ("success" :: String)]
    
instance ToJSON ValidError where
  toJSON (ValidError ms) =
    object ["result" .= ("validationError" :: String)
           ,"messages" .= ms
           ]

instance ToJSON ValidErrorMessage where
  toJSON (ValidErrorMessage key msg) =
    object ["key" .= key
           ,"message" .= msg
           ]


response500 :: LBS.ByteString -> Wai.Response
response500 = Wai.responseLBS H.status500 [("Content-Type","text/plain")]

responseTxt :: LBS.ByteString -> Wai.Response
responseTxt = Wai.responseLBS H.status200 [("Content-Type","text/plain; charset=UTF-8")]

