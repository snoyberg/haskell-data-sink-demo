{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Concurrent          (threadDelay)
import           Control.Monad               (forever)
import           Control.Monad.IO.Class      (liftIO, MonadIO)
import           Control.Monad.Logger        (logDebugN, MonadLogger)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson                  (encode, ToJSON)
import           Data.Aeson.Parser           (json)
import           Data.ByteString.Lazy        (toStrict)
import           Data.Text
import           Data.Time.Clock             (UTCTime)
import           Database.Persist.Postgresql (ConnectionString, insert, PersistEntity, PersistEntityBackend, SqlBackend, withPostgresqlConn, withPostgresqlPool)
import           Database.Persist.TH         (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import           Database.Redis              (connect, defaultConnectInfo, lrem, rpoplpush, runRedis)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Message json
  content Text
  created UTCTime default=now()
  deriving Show
 |]

--f :: Text
--f = "foobar"

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
                         liftIO $ print "Completed processing msg"
    -- pause for 250 ms
    liftIO $ threadDelay 250000

defaultDbInfo :: ConnectionString
defaultDbInfo = "host=localhost port=5432 user=postgres dbname=test password=test"

--
writeMessage ::
     ( PersistEntityBackend record ~ SqlBackend
     , MonadIO m
     , MonadBaseControl IO m
     , MonadLogger m
     , PersistEntity record
     )
  => record
  -> m ()
--writeMessage msg = withPostgresqlPool defaultDbInfo 10 $ \ conn -> do
writeMessage msg = withPostgresqlConn defaultDbInfo $ \ conn -> do
                     msgId <- runStdoutLoggingT (insert msg) (conn :: SqlBackend)
                     logDebugN "wrote msg to postgres!"

