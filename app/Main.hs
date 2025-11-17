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



-- video_info_data :: FilePath
-- video_point_data :: FilePath
-- video_info_data  = "/home/yhtq/data/cvse_video_info_data.csv"
-- video_point_data = "/home/yhtq/data/cvse_video_point_data.csv"

-- -- A text type show as Unicode string
-- newtype TextUnicode = TextUnicode Text deriving (Eq, FromField)

-- instance Show TextUnicode where
--     showsPrec :: Int -> TextUnicode -> ShowS
--     showsPrec _ (TextUnicode t) = showString (unpack t)

-- data VideoInfo = VideoInfo
--     { avid      :: !TextUnicode
--     , duration      :: !Int
--     , desc  :: !TextUnicode
--     , pubdate  :: !TextUnicode
--     , bvid  :: !TextUnicode
--     , upid  :: !(Maybe Int)
--     , video_tags  :: !TextUnicode
--     , pubdate_unix    :: !Int
--     , upname :: !TextUnicode
--     , picture :: !TextUnicode
--     } deriving (Show, Generic)

-- -- instance Show VideoInfo where
-- --     showsPrec p vi = showParen (p > 10) $
-- --         showString "VideoInfo { avid = " . showsPrec 11 (TextUnicode $ avid vi) .
-- --         showString ", duration = " . showsPrec 11 (duration vi) .
-- --         showString ", desc = " . showsPrec 11 (TextUnicode $ desc vi) .
-- --         showString ", pubdate = " . showsPrec 11 (TextUnicode $ pubdate vi) .
-- --         showString ", bvid = " . showsPrec 11 (TextUnicode $ bvid vi) .
-- --         showString ", upid = " . showsPrec 11 (upid vi) .
-- --         showString ", video_tags = " . showsPrec 11 (TextUnicode $ video_tags vi) .
-- --         showString ", pubdate_unix = " . showsPrec 11 (pubdate_unix vi) .
-- --         showString ", upname = " . showsPrec 11 (TextUnicode $ upname vi) .
-- --         showString ", picture = " . showsPrec 11 (TextUnicode $ picture vi) .
-- --         showString " }"

-- instance FromNamedRecord VideoInfo


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
main = runEff . void . runThrowPrint . runCatch  . (runInDatabase (host "127.0.0.1")) . (runInDatabaseTable "cvse_db")  . runDatabaseIO $ do
    liftIO $ putStrLn "Database setup complete."
    runDbAction run
    throw "Test error message."
    liftIO $ putStrLn "Deleted all documents from 'team' collection."