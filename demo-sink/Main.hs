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
      Left  e         -> liftIO $ putStrLn $ "ERROR!" ++ show e
      Right (Just bs) -> do
                         liftIO $ print bs
                         r <- lrem "processing" 1 bs
                         liftIO $ print r
      Right Nothing   -> liftIO $ putStrLn $ "ERROR!" ++ show msg
