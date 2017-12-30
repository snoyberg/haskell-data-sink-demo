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
import           Control.Monad.Logger        (logDebugN,
                                              logErrorN,
                                              logInfoN,
                                              MonadLogger,
                                              runStdoutLoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader  (runReaderT)
import           Data.Aeson                  (encode, ToJSON)
import           Data.Aeson.Parser           (json)
import           Data.ByteString.Lazy        (toStrict)
import           Data.Text
import           Data.Time.Clock             (UTCTime)
import           Database.Persist.Postgresql (ConnectionString,
                                              BaseBackend,
                                              insert,
                                              PersistEntity,
                                              PersistEntityBackend,
                                              PersistStoreWrite,
                                              SqlBackend,
                                              withPostgresqlConn)
import           Database.Persist.TH         (mkPersist,
                                              mkMigrate,
                                              persistLowerCase,
                                              share,
                                              sqlSettings)
import           Database.Redis              (connect,
                                              defaultConnectInfo,
                                              lrem,
                                              rpoplpush,
                                              runRedis)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Message json
  content Text
  created UTCTime default=now()
  deriving Show
 |]

defaultDbInfo :: ConnectionString
defaultDbInfo = "host=localhost port=5432 user=postgres dbname=test password=test"

--f :: Text
--f = "foobar"

saveMessage
  :: (PersistEntityBackend record ~ BaseBackend r,
      PersistStoreWrite r, MonadIO m, PersistEntity record,
      MonadLogger m) =>
     r -> record -> m ()
saveMessage conn msg = do
  msgId <- runReaderT (insert msg) conn
  logDebugN "wrote msg to postgres!"

main :: IO ()
main = runStdoutLoggingT $ do
  logInfoN $ "Data processing sink started!"
  -- connect to redis
  rconn <- liftIO $ connect defaultConnectInfo
  logInfoN $ "Connected to redis! Will loop and wait for messages.."
  -- loop forever 
  withPostgresqlConn defaultDbInfo $ \ conn -> forever $ do
    let _ = pConn :: SqlBackend
    -- retrieve a message from the queue, and save a copy to the processing table
    msg <- liftIO $ runRedis rconn $ rpoplpush "enqueued" "processing"
    case msg of
      -- failed in some way!
      Left  e         -> logErrorN $ pack $ show e
      -- no messages are in the queue
      Right Nothing   -> return () -- do nothing
      -- got a message, process it!
      Right (Just bs) -> do
                         -- "print" it to our log
                         logDebugN $ pack $ show bs
                         saveMessage (pConn :: SqlBackend) msg
                         liftIO $ print "wrote msg to postgres!"
                         -- we're done processing, drop the msg
                         r <- lrem "processing" 1 bs
                         -- print the number of rows removed
                         liftIO $ print r
    -- pause for 250 ms
    liftIO $ threadDelay 250000
