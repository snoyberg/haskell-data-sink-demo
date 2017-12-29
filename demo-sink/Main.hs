{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class
import           Data.Aeson               (Value, encode, object, (.=), FromJSON, ToJSON)
import           Data.Aeson.Parser        (json)
import           Data.ByteString.Lazy     (toStrict)
import           Database.Redis

main :: IO ()
main = do
  rconn <- connect defaultConnectInfo
  runRedis rconn $ do
    msg <- rpoplpush "enqueued" "processing"
    case msg of
      Right (Just bs) -> liftIO $ print bs --lrem "processing" 1 bs
      Right Nothing   -> liftIO $ print "ERROR!"
      Left        x   -> liftIO $ print "ERROR!"
