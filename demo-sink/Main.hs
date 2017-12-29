{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent       (threadDelay)
import           Control.Monad            (forever)
import           Control.Monad.IO.Class
import           Data.Aeson               (encode, ToJSON)
import           Data.Aeson.Parser        (json)
import           Data.ByteString.Lazy     (toStrict)
import           Database.Redis

main :: IO ()
main = do
  putStrLn $ "Data processing sink started!"
  -- connect to redis
  rconn <- connect defaultConnectInfo
  putStrLn $ "Connected to redis! Will loop and wait for messages.."
  -- loop forever running some redis commands thru the connection pool
  runRedis rconn $ forever $ do
    -- retrieve a message from the queue, and save a copy to the processing table
    msg <- rpoplpush "enqueued" "processing"
    case msg of
      -- failed in some way!
      Left  e         -> liftIO $ putStrLn $ "ERROR!" ++ show e
      -- no messages are in the queue
      Right Nothing   -> return () -- do nothing
      -- got a message, process it!
      Right (Just bs) -> do
                         -- "print" it to our log
                         liftIO $ print bs
                         -- we're done processing, drop the msg
                         r <- lrem "processing" 1 bs
                         -- print the number of rows removed
                         liftIO $ print r
    -- pause for 250 ms
    liftIO $ threadDelay 250000
