{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module CVSEDatabase where
import Database.MongoDB
import Logger
import qualified Data.Time.Clock as TC
import qualified Data.Time.LocalTime as TCL
import Data.Time.Format.ISO8601 (calendarFormat, formatShow, FormatExtension(BasicFormat))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import DatabaseProvider
import Data.Text (pack, Text, append)
import Capnp.Gen.CVSEAPI.CVSE
import qualified Capnp as C
import Capnp (Raw, Mutability(..))
import Control.Monad.Hefty (Eff, (:>))
import Control.Monad (join)
import Data.List (nub)



-- 数据库结构：
-- video_metadata_collection:
--   存储视频的基本信息，如视频ID、标题、描述、上传时间等。
--   我们使用 bvid 字符串（包含 “BV” 前缀）作为 _id
--   使用 "examined" 字段表示该视频是否已经被处理过（布尔值）。
-- video_stats_collection:
--   存储视频的统计数据，如播放量、点赞数、评论数等。
--   使用 "date" 记录采集时间（BSON data 类型），自动创建 id 字段。

uft8 :: TCL.TimeZone
uft8 = TCL.hoursToTimeZone 8

joinJust :: [Maybe a] -> [a]
joinJust [] = []
joinJust (x:xs) = case x of
    Just v -> v : joinJust xs
    Nothing -> joinJust xs

videoMetadataCollection :: Collection
videoMetadataCollection = "video_metadata_collection"

videoStatsCollection :: Collection
videoStatsCollection = "video_stats_collection"


insertNewVideo :: Parsed Cvse'RecordingNewEntry -> Document
insertNewVideo entry =
    [
        "_id" =: entry.bvid,
        "avid" =: entry.avid,
        "title" =: entry.title,
        "uploader" =: entry.uploader,
        "up_face" =: entry.upFace,
        "copyright" =: entry.copyright,
        "pubdate" =: entry.pubdate,
        "duration" =: entry.duration,
        "page" =: entry.page,
        "cover" =: entry.cover,
        "desc" =: entry.desc,
        "tags" =: entry.tags,
        "is_examined" =: False
    ]

insertNewStats :: Parsed Cvse'RecordingDataEntry -> Document
insertNewStats entry = 
    [
        "bvid" =: entry.bvid,
        "avid" =: entry.avid,
        "view" =: entry.view,
        "favorite" =: entry.favorite,
        "coin" =: entry.coin,
        "like" =: entry.like,
        "share" =: entry.share,
        "danmaku" =: entry.danmaku,
        "reply" =: entry.reply,
        "share" =: entry.share,
        "date" =: posixSecondsToUTCTime (fromIntegral entry.date)
    ]

inDomesticField :: Field
inDomesticField = "in_domestic" =: True

notInDomesticField :: Field
notInDomesticField = "in_domestic" =: False

inSVField :: Field
inSVField = "in_sv" =: True

notInSVField :: Field
notInSVField = "in_sv" =: False

inUTAUField :: Field
inUTAUField = "in_utau" =: True

notInUTAUField :: Field
notInUTAUField = "in_utau" =: False

genRank :: [Parsed Cvse'Rank] -> Document  
genRank rs = 
    let (in_dom, in_sv, in_utau) = foldl (\(in_dom, in_sv, in_utau) rank ->
            case rank.value of
                Cvse'Rank'RankValue'domestic -> (in_dom || True, in_sv, in_utau)
                Cvse'Rank'RankValue'sv -> (in_dom, in_sv || True, in_utau)
                Cvse'Rank'RankValue'utau -> (in_dom, in_sv, in_utau || True)
            ) (False, False, False) rs
    in [
            if in_dom then inDomesticField else notInDomesticField,
            if in_sv then inSVField else notInSVField,
            if in_utau then inUTAUField else notInUTAUField
        ]

updateModifyRank :: Parsed Cvse'ModifyEntry -> (Selector, Document, [UpdateOption])
updateModifyRank entry =
    ( ["_id" =: entry.bvid]
    , join $ joinJust [
        if entry.hasRanks
            then Just (genRank entry.ranks)
            else Nothing,
        if entry.hasIsRepublish
            then Just ["is_republish" =: entry.isRepublish]
            else Nothing,
        if entry.hasStaffInfo
            then Just ["staff" =: entry.staffInfo]
            else Nothing
    ] 
    , []
    )

