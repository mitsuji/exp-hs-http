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


import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import Data.Aeson.Types (ToJSON,toJSON,object,(.=))
import qualified Data.Aeson as AE

import Data.Char (isDigit)
import Data.Either (partitionEithers)

import qualified Data.Yaml as Yaml

import Data.Aeson.Types (FromJSON,parseJSON,Value(..),(.:))

data Conf = Conf { confHost :: String
                 , confPort :: Int
                 , confRoot :: String
                 , confRdbHost :: String
                 , confRdbName :: String
                 , confRdbUser :: String
                 , confRdbPass :: String
                 }


instance FromJSON Conf where
  parseJSON (Object o) = Conf <$> o .: "host"
                              <*> o .: "port"
                              <*> o .: "root"
                              <*> o .: "rdbHost"
                              <*> o .: "rdbName"
                              <*> o .: "rdbUser"
                              <*> o .: "rdbPass"
  parseJSON _ = mempty


main :: IO ()
main = do
  confPath:_ <- getArgs
  econf <- Yaml.decodeFileEither confPath
  case econf  of
    Left error -> putStrLn $ show error
    Right conf -> do
      Warp.runSettings (
        Warp.setHost (fromString $ confHost conf) $
        Warp.setPort (confPort conf) $
        Warp.defaultSettings
        ) $ routerApp $ confRoot conf


routerApp :: String -> Wai.Application
routerApp root req respond =
  let
    apppath = "exp-hs-http"
  in case (M.parseMethod (Wai.requestMethod req), Wai.pathInfo req) of
    (Right M.POST, [apppath, "ep1"])  -> ep1App req respond -- /exp-hs-http/ep1
    (Right M.POST, [apppath, "ep2"])  -> respond $ responseTxt "ep2" -- /exp-hs-http/ep2
    _ -> staticApp root req respond -- static html/js/css files


{--
curl -X POST \
  -d "name=三辻尚栄" \
  -d "namekana=みつじたかまさ" \
  -d "zipcode=9110034" \
  -d "pref=福井県" \
  -d "addr1=福井市和田東 1-222" \
  -d "addr2=SYビル C" \
  -d "tel=0776583380" \
  http://localhost:8080/exp-hs-http/ep1 | jq .
--}
ep1App :: Wai.Application
ep1App req respond = do
  (ps,_) <- Parse.parseRequestBody Parse.lbsBackEnd req
  case partitionEithers $ fmap (\f->f ps)
       [validName,validNameKana,validZipcode,validPref,validAddr1,validAddr2,validTel] of
    ([],ts) -> do
      mapM_ (\(k,v) -> BS.putStr k >> putStr ": " >> T.putStrLn v) ts  -- [TODO] execution
      respond $ responseTxt $ AE.encode Success
    (es,_)  -> respond $ response500 $ AE.encode $ ValidError es
  where
    
    validName :: [Parse.Param] -> Either ValidErrorMessage (BS.ByteString,T.Text)
    validName ps =
      let
        key = "name"
      in case lookup key ps of
        Nothing -> Left $ ValidErrorMessage key "名前が指定されていません"
        Just bs -> case T.decodeUtf8 bs of
          ts | T.length ts == 0 -> Left $ ValidErrorMessage key "名前が指定されていません"
          ts | T.length ts > 20 -> Left $ ValidErrorMessage key "名前は20文字以内で入力してください"
          ts -> Right (key, ts)

    validNameKana :: [Parse.Param] -> Either ValidErrorMessage (BS.ByteString,T.Text)
    validNameKana ps =
      let
        key = "namekana"
      in case lookup key ps of
        Nothing -> Left $ ValidErrorMessage key "名前(かな)が指定されていません"
        Just bs -> case T.decodeUtf8 bs of -- [TODO] かなチェック
          ts | T.length ts == 0 -> Left $ ValidErrorMessage key "名前(かな)が指定されていません"
          ts | T.length ts > 40 -> Left $ ValidErrorMessage key "名前(かな)は40文字以内で入力してください"
          ts -> Right (key, ts)

    validZipcode :: [Parse.Param] -> Either ValidErrorMessage (BS.ByteString,T.Text)
    validZipcode ps =
      let
        key = "zipcode"
      in case lookup key ps of
        Nothing -> Left $ ValidErrorMessage key "郵便番号が指定されていません"
        Just bs -> case T.decodeUtf8 bs of -- [TODO] ハイフンを許可
          ts | T.length ts /= 7       -> Left $ ValidErrorMessage key "郵便番号は7桁で指定してください"
          ts | not (T.all isDigit ts) -> Left $ ValidErrorMessage key "郵便番号は半角数字で指定してください"
          ts -> Right (key, ts)

    validPref :: [Parse.Param] -> Either ValidErrorMessage (BS.ByteString,T.Text)
    validPref ps =
      let
        key = "pref"
      in case lookup key ps of
        Nothing -> Left $ ValidErrorMessage key "都道府県が指定されていません"
        Just bs -> case T.decodeUtf8 bs of
          ts | T.length ts == 0          -> Left $ ValidErrorMessage key "都道府県が指定されていません"
          ts | not (elem ts prefectures) -> Left $ ValidErrorMessage key "都道府県が正しくありません"
          ts -> Right (key, ts)
          
    validAddr1 :: [Parse.Param] -> Either ValidErrorMessage (BS.ByteString,T.Text)
    validAddr1 ps =
      let
        key = "addr1"
      in case lookup key ps of
        Nothing -> Left $ ValidErrorMessage key "住所1が指定されていません"
        Just bs -> case T.decodeUtf8 bs of
          ts | T.length ts == 0 -> Left $ ValidErrorMessage key "住所1が指定されていません"
          ts | T.length ts > 20 -> Left $ ValidErrorMessage key "住所1は20文字以内で入力してください"
          ts -> Right (key, ts)

    validAddr2 :: [Parse.Param] -> Either ValidErrorMessage (BS.ByteString,T.Text)
    validAddr2 ps =
      let
        key = "addr2"
      in case lookup key ps of
        Nothing -> Left $ ValidErrorMessage key "住所2が指定されていません"
        Just bs -> case T.decodeUtf8 bs of
          ts | T.length ts == 0 -> Left $ ValidErrorMessage key "住所2が指定されていません"
          ts | T.length ts > 20 -> Left $ ValidErrorMessage key "住所2は20文字以内で入力してください"
          ts -> Right (key, ts)

    validTel :: [Parse.Param] -> Either ValidErrorMessage (BS.ByteString,T.Text)
    validTel ps =
      let
        key = "tel"
      in case lookup key ps of
        Nothing -> Left $ ValidErrorMessage key "電話番号が指定されていません"
        Just bs -> case T.decodeUtf8 bs of -- [TODO] ハイフンを許可
          ts | T.length ts == 0 -> Left $ ValidErrorMessage key "電話番号が指定されていません"
          ts | T.length ts > 20 -> Left $ ValidErrorMessage key "電話番号は20文字以内で入力してください"
          ts | not (T.all isDigit ts) -> Left $ ValidErrorMessage key "電話番号は半角数字で指定してください"
          ts -> Right (key, ts)



staticApp :: String -> Wai.Application
staticApp root = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.defaultWebAppSettings root
    indices = fromJust $ toPieces ["main.html"] -- default content



data Success = Success
data ValidError = ValidError [ValidErrorMessage]
data ValidErrorMessage = ValidErrorMessage BS.ByteString T.Text

instance ToJSON Success where
  toJSON (Success) =
    object ["result" .= ("success" :: String)]
    
instance ToJSON ValidError where
  toJSON (ValidError ms) =
    object ["result" .= ("validError" :: String)
           ,"messages" .= ms
           ]

instance ToJSON ValidErrorMessage where
  toJSON (ValidErrorMessage key msg) =
    object ["key" .= T.decodeUtf8 key
           ,"message" .= msg
           ]


response500 :: LBS.ByteString -> Wai.Response
response500 = Wai.responseLBS H.status500 [("Content-Type","text/plain")]

responseTxt :: LBS.ByteString -> Wai.Response
responseTxt = Wai.responseLBS H.status200 [("Content-Type","application/json; charset=UTF-8")]



prefectures :: [T.Text]
prefectures = [ "北海道"
                , "青森県"
                , "岩手県"
                , "宮城県"
                , "秋田県"
                , "山形県"
                , "福島県"
                , "茨城県"
                , "栃木県"
                , "群馬県"
                , "埼玉県"
                , "千葉県"
                , "東京都"
                , "神奈川県"
                , "新潟県"
                , "富山県"
                , "石川県"
                , "福井県"
                , "山梨県"
                , "長野県"
                , "岐阜県"
                , "静岡県"
                , "愛知県"
                , "三重県"
                , "滋賀県"
                , "京都府"
                , "大阪府"
                , "兵庫県"
                , "奈良県"
                , "和歌山県"
                , "鳥取県"
                , "島根県"
                , "岡山県"
                , "広島県"
                , "山口県"
                , "徳島県"
                , "香川県"
                , "愛媛県"
                , "高知県"
                , "福岡県"
                , "佐賀県"
                , "長崎県"
                , "熊本県"
                , "大分県"
                , "宮崎県"
                , "鹿児島県"
                , "沖縄県"
                ]

