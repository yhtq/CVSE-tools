{-# LANGUAGE DerivingVia #-}
module Main (main) where

import Prelude hiding (show, putStrLn, print)
import Data.Csv
import GHC.Generics (Generic)
import Data.Text (Text, show, unpack)
import Data.Vector qualified as V
import Data.Text.IO (putStrLn)
import Data.Text.IO.Utf8 qualified as TIO
import CsvProvider
import Control.Monad.Hefty (runEff, liftIO)

video_info_data :: FilePath
video_point_data :: FilePath
video_info_data  = "/home/yhtq/data/cvse_video_info_data.csv"
video_point_data = "/home/yhtq/data/cvse_video_point_data.csv"

-- A text type show as Unicode string
newtype TextUnicode = TextUnicode Text deriving (Eq, FromField)

instance Show TextUnicode where
    showsPrec :: Int -> TextUnicode -> ShowS
    showsPrec _ (TextUnicode t) = showString (unpack t)

data VideoInfo = VideoInfo
    { avid      :: !TextUnicode
    , duration      :: !Int
    , desc  :: !TextUnicode
    , pubdate  :: !TextUnicode
    , bvid  :: !TextUnicode
    , upid  :: !(Maybe Int)
    , video_tags  :: !TextUnicode
    , pubdate_unix    :: !Int
    , upname :: !TextUnicode
    , picture :: !TextUnicode
    } deriving (Show, Generic)

-- instance Show VideoInfo where
--     showsPrec p vi = showParen (p > 10) $
--         showString "VideoInfo { avid = " . showsPrec 11 (TextUnicode $ avid vi) .
--         showString ", duration = " . showsPrec 11 (duration vi) .
--         showString ", desc = " . showsPrec 11 (TextUnicode $ desc vi) .
--         showString ", pubdate = " . showsPrec 11 (TextUnicode $ pubdate vi) .
--         showString ", bvid = " . showsPrec 11 (TextUnicode $ bvid vi) .
--         showString ", upid = " . showsPrec 11 (upid vi) .
--         showString ", video_tags = " . showsPrec 11 (TextUnicode $ video_tags vi) .
--         showString ", pubdate_unix = " . showsPrec 11 (pubdate_unix vi) .
--         showString ", upname = " . showsPrec 11 (TextUnicode $ upname vi) .
--         showString ", picture = " . showsPrec 11 (TextUnicode $ picture vi) .
--         showString " }"

instance FromNamedRecord VideoInfo

main :: IO ()
main = runEff . csvIOToIO $ do
    video_info_datas <- readCsv @VideoInfo video_info_data
    liftIO $ putStrLn $ "CSV data read successfully with length: " <> show (V.length video_info_datas)
    liftIO $ putStrLn $ show (V.take 5 video_info_datas)
