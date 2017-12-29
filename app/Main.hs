{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception        (SomeException)
import           Control.Exception.Lifted (handle)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (Value, encode, object, (.=), FromJSON, ToJSON)
import           Data.Aeson.Parser        (json)
import           Data.ByteString          (ByteString)
import           Data.Conduit             (($$))
import           Data.Conduit.Attoparsec  (sinkParser)
import           Data.Text
import           Database.Redis
import           GHC.Generics
import           Network.HTTP.Types       (status200, status400)
import           Network.Wai              (Application, Response, responseLBS, pathInfo)
import           Network.Wai.Conduit      (sourceRequestBody)
import           Network.Wai.Handler.Warp (run)

data StatusMsg = StatusMsg { status :: Text } deriving (Generic, Show)

instance FromJSON StatusMsg
instance ToJSON   StatusMsg

main :: IO ()
main = run 2378 app

app :: Application
app req sendResponse = handle (sendResponse . invalidJson) $ do
    case pathInfo req of
      [] -> do
        value <- sourceRequestBody req $$ sinkParser json
        newValue <- liftIO $ modValue value
        runRedis rconn $ lpush "enqueued" value
        sendResponse $ responseLBS
            status200
            [("Content-Type", "application/json")]
            $ encode newValue
      ["status"] -> do
        sendResponse $ responseLBS
            status200 --[] "status: online"
            [("Content-Type", "application/json")]
            $ encode (StatusMsg { status = "online" })
      _ -> do
        sendResponse $ responseLBS
            status200 [] "hello world!"
    where
      rconn = connect defaultConnectInfo

invalidJson :: SomeException -> Response
invalidJson ex = responseLBS
    status400
    [("Content-Type", "application/json")]
    $ encode $ object
        [ ("message" .= show ex)
        ]

-- Application-specific logic would go here.
modValue :: Value -> IO Value
modValue = return
