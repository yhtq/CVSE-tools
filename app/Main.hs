{-# LANGUAGE DerivingVia #-}
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
import Control.Monad.Hefty (runEff, liftIO, raise, runPure)
import Database.MongoDB 
import Control.Monad.Hefty.Except (throw, runCatch)
import Control.Monad(void)
import Capnp (SomeServer, def, defaultLimit, export, handleParsed)
import Capnp.Rpc (ConnConfig (..), handleConn, socketTransport, toClient)
import Network.Simple.TCP (serve)
import Supervisors (withSupervisor)
import Logger (outputLoggerToFile, fmtLogger)
import Colog (logError, logDebug, logInfo, logWarning)

import Capnp.Gen.CVSEAPI.CVSE (Cvse'server_(..))

logFilePath :: FilePath
logFilePath = "cvse_log"

data MyEchoServer = MyEchoServer

instance SomeServer MyEchoServer

instance Cvse'server_ MyEchoServer where
   cvse'updateModifyEntry _ = handleParsed (\entries -> return def)
   cvse'updateNewEntry _ = handleParsed (\entries -> return def)
   cvse'updateAddressingDataEntry _ = handleParsed (\entries -> return def)


run :: Action IO ()
run = do
   clearTeams
   insertTeams
   allTeams >>= printDocs "All Teams"
   nationalLeagueTeams >>= printDocs "National League Teams"
   newYorkTeams >>= printDocs "New York Teams"
   clearTeams

clearTeams :: Action IO ()
clearTeams = delete (select [] "team")

insertTeams :: Action IO [Value]
insertTeams = insertMany "team" [
   ["name" =: "Yankees", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "American"],
   ["name" =: "Mets", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "National"],
   ["name" =: "Phillies", "home" =: ["city" =: "Philadelphia", "state" =: "PA"], "league" =: "National"],
   ["name" =: "Red Sox", "home" =: ["city" =: "Boston", "state" =: "MA"], "league" =: "American"] ]

allTeams :: Action IO [Document]
allTeams = rest =<< find (select [] "team") {sort = ["home.city" =: 1]}

nationalLeagueTeams :: Action IO [Document]
nationalLeagueTeams = rest =<< find (select ["league" =: "National"] "team")

newYorkTeams :: Action IO [Document]
newYorkTeams = rest =<< find (select ["home.state" =: "NY"] "team") {project = ["name" =: 1, "league" =: 1]}

printDocs :: Text -> [Document] -> Action IO ()
printDocs title docs = liftIO $ putStrLn title >> mapM_ (print . exclude ["_id"]) docs

main :: IO ()
main = runEff . void . 
   runThrowPrint . 
   outputLoggerToFile logFilePath . fmtLogger . 
   runCatch . 
   runInDatabase (host "127.0.0.1") . 
   runInDatabaseTable "cvse_db"  . 
   runDatabaseIO $ do
    liftIO $ putStrLn "Database setup complete."
    runDbAction run
    throw "Test error message."
    liftIO $ putStrLn "Deleted all documents from 'team' collection."