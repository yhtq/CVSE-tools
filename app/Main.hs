{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main (main) where

import Prelude hiding (show, putStrLn)
import Data.Csv
import GHC.Generics (Generic)
import Data.Text (Text, show, unpack)
import Data.Vector qualified as V
import Data.Text.IO (putStrLn)
import Data.Text.IO.Utf8 qualified as TIO
import CsvProvider
import DatabaseProvider
import Control.Monad.Hefty (runEff, liftIO, raise, runPure, Eff, Emb, Throw, Catch, Ask)
import Database.MongoDB
import Control.Monad.Hefty.Except (throw, runCatch)
import Control.Monad(void)
import Capnp (SomeServer, def, defaultLimit, export, handleParsed)
import Capnp.Rpc (ConnConfig (..), handleConn, socketTransport, toClient, throwFailed)
import Network.Simple.TCP (serve)
import Supervisors (withSupervisor)
import Logger
import Colog (Severity (..))

import Capnp.Gen.CVSEAPI.CVSE
import CVSEDatabase
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

logFilePath :: FilePath
logFilePath = "cvse_service.log"

data MyServer = MyServer {
   peer :: Text,
   databaseServer :: Host,
   databasePort :: Maybe Int,
   databaseName :: Database
}

instance SomeServer MyServer

mainHandler :: Host -> Database -> Eff '[
   DatabaseIO,
   DataBaseTableState,
   LogE,
   Throw ErrMessage,
   Emb IO] a -> IO (Either ErrMessage a)
mainHandler host database  = runEff .
         runThrow .
         runCatch .
         defaultLoggerHandler logFilePath Debug .
         runCatch .
         runInDatabase host .
         runInDatabaseTable database  .
         runDatabaseIO

mainWrapper :: MyServer -> Eff '[
   DatabaseIO,
   DataBaseTableState,
   LogE,
   Throw ErrMessage,
   Emb IO] a -> IO a
mainWrapper server action = do
   result <- mainHandler server.databaseServer server.databaseName action
   case result of
      Left err -> throwFailed err
      Right val -> return val

instance Cvse'server_ MyServer where
   cvse'updateModifyEntry server = handleParsed (
      \param -> mainWrapper server $ do
            $(logInfo) $ "Received ModifyRankEntry update from client: " <> server.peer <> "with " <> show (length param.entries) <> " entries."
            runDbAction $
               updateMany videoMetadataCollection (fmap updateModifyRank param.entries)
            return def
      )
   cvse'updateNewEntry server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) $ "Received RecordingNewEntry from client: " <> server.peer <> "with " <> show (length param.entries) <> " entries."
         runDbAction $ do
            insertMany_ videoMetadataCollection (fmap insertNewVideo param.entries)
         return def
      )
   cvse'updateRecordingDataEntry server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) $ "Received RecordingDataEntry update from client: " <> server.peer <> "with " <> show (length param.entries) <> " entries."
         runDbAction $ do
            insertMany_ videoStatsCollection (fmap insertNewStats param.entries)
         return def
      )
   cvse'getAll server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) $ "Received getAll request from client: " 
            <> server.peer <> " with get_unexamined=" <> show param.get_unexamined 
            <> " and get_unincluded=" <> show param.get_unincluded
         docs <- runDbAction $ do
            let baseQuery = getAllMetaInfo param.get_unexamined param.get_unincluded
            find (select baseQuery videoMetadataCollection) >>= rest
         let indices = fmap (\doc -> Cvse'Index {
               bvid = at "_id" doc,
               avid = at "avid" doc
            }) docs
         $(logInfo) $ "getAll found " <> show (length indices) <> " entries."
         return (Cvse'getAll'results { indices = indices })
      )
   cvse'lookupMetaInfo server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) $ "Received lookupMetaInfo request from client: " <> server.peer <> " with " <> show (length param.indices) <> " indices."
         docs <- mapM (\index -> do
               result <- runDbAction $ findOne (select (lookupMetaInfoQuery index) videoMetadataCollection)
               case result of
                  Just doc -> return doc
                  Nothing -> throw $ "Video with bvid " <> index.bvid <> " and avid " <> index.avid <> " not found."
               ) param.indices
         let entries = fmap parseRecordingNewEntry docs
         return (Cvse'lookupMetaInfo'results { entries = entries })
         )
   cvse'lookupDataInfo server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) $ "Received lookupDataInfo request from client: " <> server.peer <> " with " <> show (length param.indices) <> " indices."
         docs <- mapM (\index -> runDbAction $
                  find (select (lookupDataInfoQuery index param.from_date param.to_date) videoStatsCollection)
                  >>= rest
               ) param.indices
         let entries = fmap (fmap parseRecordingDataEntry) docs
         return (Cvse'lookupDataInfo'results { entries = entries })
         )

   cvse'lookupOneDataInfo server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) $ "Received lookupOneDataInfo request from client: " <> server.peer <> " with " <> show (length param.indices) <> " indices."
         docs <- mapM (\index -> do
               result <- runDbAction $ findOne (select (lookupDataInfoQuery index param.from_date param.to_date) videoStatsCollection)
               case result of
                  Just doc -> return (parseRecordingDataEntry doc)
                  Nothing -> throw $ "No data found for video with bvid " <> index.bvid <> " and avid " <> index.avid <> " in the specified date range."
               ) param.indices
         return (Cvse'lookupOneDataInfo'results { entries = docs })
      )

serverAddr = "localhost"

serverPort  = "8663"

main :: IO ()
main = withSupervisor  $ \sup -> do
   let dbHost = host "127.0.0.1"
   let dbPort = Nothing
   let dbName = "cvse_db"
   let server = MyServer {
         peer = "",
         databaseServer = dbHost,
         databasePort = dbPort,
         databaseName = dbName
      }
   putStrLn $ "Starting CVSE RPC server at " <> show serverAddr <> ":" <> show serverPort
   serve serverAddr serverPort $ \(sock, addr) -> do
      let server1 = server { peer = show addr <> " " <> show sock }
      boot <- export @Cvse sup server1
      handleConn
         (socketTransport sock defaultLimit)
         def {
            debugMode = False,
            bootstrap = Just (toClient boot)
         }
