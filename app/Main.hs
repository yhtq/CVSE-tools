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
import GHC.Base (when)

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
         defaultLoggerHandler logFilePath (Just Info) .
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
            $(logInfo) $ "Received ModifyRankEntry update from client: " <> server.peer <> " with " <> show (length param.entries) <> " entries."
            runDbAction $
               updateMany videoMetadataCollection (fmap updateModifyRank param.entries)
            return def
      )
   cvse'updateNewEntry server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) $ "Received RecordingNewEntry from client: " <> server.peer <> " with " <> show (length param.entries) <> " entries."
         writeResult <- runDbAction $ do
            let insertFun = if param.replace
                  then \docs -> do
                     writeResult <- updateMany videoMetadataCollection $ fmap (
                           \doc ->
                              (["_id" =: (at "_id" doc :: Text)], doc, [Upsert])
                        ) docs
                     return $ show writeResult
                  else fmap show . insertAll videoMetadataCollection 
            insertFun (fmap insertNewVideo param.entries)
         if param.replace then
            $(logDebug) $ "Database upsert result: " <> show writeResult
         else
            $(logDebug) $ "Database insert result: " <> show writeResult
         return def
      )
   cvse'updateRecordingDataEntry server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) $ "Received RecordingDataEntry update from client: " <> server.peer <> " with " <> show (length param.entries) <> " entries."
         runDbAction $ do
            insertMany_ videoStatsCollection (fmap insertNewStats param.entries)
         return def
      )
   cvse'getAll server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) $ "Received getAll request from client: "
            <> server.peer <> " with get_unexamined=" <> show param.get_unexamined
            <> ", get_unincluded=" <> show param.get_unincluded
            <> ", from_date=" <> show param.from_date
            <> ", to_date=" <> show param.to_date <> "."
         let baseQuery = getAllMetaInfo
                  param.get_unexamined param.get_unincluded
                  param.from_date param.to_date
         $(logDebug) $ "Base query: " <> show baseQuery
         indices_maybe  <- runDbAction $ do
            find ((select baseQuery videoMetadataCollection) {project = ["_id" =: 1, "avid" =: 1]})
               >>= rest >>= mapM (\doc -> return (doc !? "_id", doc !? "avid", doc))
         indices <- mapM (\(mbBvid, mbAvid, doc) -> case (mbBvid, mbAvid) of
               (Just (String bvid), Just (String avid)) -> do
                  $(logDebug) $ "Found index: bvid=" <> bvid <> ", avid=" <> avid
                  return Cvse'Index {
                     bvid = bvid,
                     avid = avid
                  }
               _ -> throw $ "Malformed document found in video_metadata_collection: " <> show doc
            ) indices_maybe
         $(logInfo) $ "getAll found " <> show (length indices) <> " entries."
         return (Cvse'getAll'results { indices = indices })
      )
   cvse'lookupMetaInfo server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) $ "Received lookupMetaInfo request from client: " <> server.peer <> " with " <> show (length param.indices) <> " indices."
         entries <- runDbAction $ find (select (lookupMetaInfoQuery param.indices) videoMetadataCollection)
                  >>= mapCursorBatch parseRecordingNewEntry
         return (Cvse'lookupMetaInfo'results { entries = entries })
         )
   cvse'lookupDataInfo server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) $ "Received lookupDataInfo request from client: " <> server.peer <> " with " <> show (length param.indices) <> " indices."
         entries <- mapM (\index -> runDbAction $
                  find (select (lookupDataInfoQuery index param.from_date param.to_date) videoStatsCollection)
                  >>= mapCursorBatch parseRecordingDataEntry
               ) param.indices
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
   
   cvse'reCalculateRankings server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) 
            $ "Received reCalculateRankings request from client: " <> server.peer 
            <> " for rank " <> show param.rank <> ", index " <> show param.index 
            <> ", contain_unexamined=" <> show param.contain_unexamined
            <> ", lock=" <> show param.lock <> "."
         let collectionName = genCollectionName $ EachRankingConfig {
            rank = param.rank,
            index = fromIntegral param.index,
            containUnexamined = param.contain_unexamined
         }
         isLock <- runDbAction $ checkLock collectionName
         when isLock $
            throw $ "Collection " <> collectionName <> " is LOCKED. Aborting ranking calculation."
         runDbAction $ unlockCollection collectionName   -- Warning: Debug only, remove in production
         $(logWarning) $ "Warning: unlockCollection called unconditionally for collection " <> collectionName <> ". This is for debugging only, remove in production!"
         let (period1, period2) = genPeriod param.rank (fromIntegral param.index)
         $(logInfo) $ "Calculating rankings for period from " <> show period1 <> " to " <> show period2 <> "."
         let rankingConfig = EachRankingConfig {
            rank = param.rank,
            index = fromIntegral param.index,
            containUnexamined = param.contain_unexamined
         }
         let selector = genRankingQuery rankingConfig
         $(logDebug) $ "Ranking query selector: " <> show selector
         runDbAction $ do
            dropCollection collectionName
            cursor <- find ((select selector videoMetadataCollection) {project = ["_id" =: 1]})  
            docs <- rest cursor
            let bvids = map (at "_id") docs
            calculateVideoPoints bvids collectionName period1 period2
         processSHAndHot rankingConfig
         if param.lock then do
            $(logInfo) $ "Locked collection " <> collectionName <> " after finishing ranking calculation."
         else do
            runDbAction $ unlockCollection collectionName
            $(logInfo) $ "finished ranking calculation and unlocked collection " <> collectionName <> "."
         return (Cvse'reCalculateRankings'results { })
      )
   
   cvse'getAllRankingInfo server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) $ "Received getAllRankingInfo request from client: " <> server.peer <> " for rank " <> show param.rank <> ", index " <> show param.index <> ", contain_unexamined=" <> show param.contain_unexamined <> "."
         let collectionName = genCollectionName $ EachRankingConfig {
            rank = param.rank,
            index = fromIntegral param.index,
            containUnexamined = param.contain_unexamined
         }
         entries <- runDbAction $ do
            getRankInfo (fromIntegral param.from_rank) (fromIntegral param.to_rank) collectionName 
            >>= mapCursorBatch (\doc -> Cvse'Index {
               bvid = at "_id" doc,
               avid = at "avid" doc
            })
         $(logInfo) $ "getAllRankingInfo found " <> show (length entries) <> " entries."
         return (Cvse'getAllRankingInfo'results { entries = entries })
      )
   
   cvse'lookupRankingInfo server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) $ "Received lookupRankingInfo request from client: " <> server.peer <> " for rank " <> show param.rank <> ", contain_unexamined=" <> show param.contain_unexamined 
            <> "with " <> show (length param.indices) <> " indices."
         let collectionName = genCollectionName $ EachRankingConfig {
            rank = param.rank,
            index = fromIntegral param.index,
            containUnexamined = param.contain_unexamined
         }
         let recentTwoWeeksCountCollection = genRecentTenWeeksOnMainCountingCollectionName $ EachRankingConfig {
            rank = param.rank,
            index = fromIntegral param.index,
            containUnexamined = param.contain_unexamined
         }
         entriesM <- runDbAction $ do
            find (select (lookupRankingInfoQuery param.indices) collectionName)
            >>= mapCursorBatch (parseRankingInfoEntry recentTwoWeeksCountCollection)
         entries <- sequence entriesM
         $(logInfo) $ "lookupRankingInfo found " <> show (length entries) <> " entries."
         return (Cvse'lookupRankingInfo'results { entries = entries })
      )
   
   cvse'lookupRankingMetaInfo server = handleParsed (
      \param -> mainWrapper server $ do
         $(logInfo) $ "Received lookupRankingMetaInfo request from client: " <> server.peer <> " for rank " <> show param.rank <> ", index " <> show param.index 
            <> ", contain_unexamined=" <> show param.contain_unexamined <> "."
         let query = lookupRankingMetaInfoQuery param.rank (fromIntegral param.index) param.contain_unexamined
         result <- runDbAction $ findOne query
         case result of
            Just doc -> return (Cvse'lookupRankingMetaInfo'results {
               stat = parseRankingMetaInfoStat doc
            })
            Nothing -> throw $ "No ranking meta info found for rank " <> show param.rank <> ", index " <> show param.index 
               <> ", contain_unexamined=" <> show param.contain_unexamined <> "."
      )

serverAddr = "0.0.0.0"

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
