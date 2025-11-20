{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module CVSEDatabase where
import Database.MongoDB
import Logger
import qualified Data.Time.Clock as TC
import qualified Data.Time.LocalTime as TCL
import Data.Time.Format.ISO8601 (calendarFormat, formatShow, FormatExtension(BasicFormat))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import DatabaseProvider
import Data.Text (pack, Text, append)
import Capnp.Gen.CVSEAPI.CVSE
import qualified Capnp as C
import Capnp (Raw, Mutability(..))
import Control.Monad.Hefty (Eff, (:>))
import Control.Monad (join)
import Data.List (nub)
import Data.Time.Clock.System (SystemTime (..), utcToSystemTime, systemToUTCTime)



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

filterJust :: [Maybe a] -> [a]
filterJust [] = []
filterJust (x:xs) = case x of
    Just v -> v : filterJust xs
    Nothing -> filterJust xs


videoMetadataCollection :: Collection
videoMetadataCollection = "video_metadata_collection"

videoStatsCollection :: Collection
videoStatsCollection = "video_stats_collection"

parseSystemTime :: Parsed Cvse'Time -> SystemTime
parseSystemTime time = MkSystemTime time.seconds (fromIntegral time.nanoseconds)

buildTime :: SystemTime -> Parsed Cvse'Time
buildTime (MkSystemTime sec nsec) = Cvse'Time {
        seconds = sec,
        nanoseconds = fromIntegral nsec
    }

insertNewVideo :: Parsed Cvse'RecordingNewEntry -> Document
insertNewVideo entry =
    [
        "_id" =: entry.bvid,
        "avid" =: entry.avid,
        "title" =: entry.title,
        "uploader" =: entry.uploader,
        "up_face" =: entry.upFace,
        "copyright" =: entry.copyright,
        "pubdate" =: (systemToUTCTime . parseSystemTime) entry.pubdate,
        "duration" =: entry.duration,
        "page" =: entry.page,
        "cover" =: entry.cover,
        "desc" =: entry.desc,
        "tags" =: entry.tags,
        "is_examined" =: entry.isExamined
    ] 
        ++ genRank entry.ranks ++
    [
        "is_republish" =: entry.isRepublish,
        "staff" =: entry.staffInfo
    ]

parseRanks :: Document -> [Parsed Cvse'Rank]
parseRanks doc = filterJust [
        if at "in_domestic" doc
            then Just Cvse'Rank {
                value = Cvse'Rank'RankValue'domestic
            }
            else Nothing,
        if at "in_sv" doc
            then Just Cvse'Rank {
                value = Cvse'Rank'RankValue'sv
            }
            else Nothing,
        if at "in_utau" doc
            then Just Cvse'Rank {
                value = Cvse'Rank'RankValue'utau
            }
            else Nothing
    ]

parseRecordingNewEntry :: Document -> Parsed Cvse'RecordingNewEntry
parseRecordingNewEntry doc = Cvse'RecordingNewEntry {
        bvid = at "_id" doc,
        avid = at "avid" doc,
        title = at "title" doc,
        uploader = at "uploader" doc,
        upFace = at "up_face" doc,
        copyright = at "copyright" doc,
        pubdate = buildTime . utcToSystemTime $ at "pubdate" doc,
        duration = at "duration" doc,
        page = at "page" doc,
        cover = at "cover" doc,
        desc = at "desc" doc,
        tags = at "tags" doc,
        isExamined = at "is_examined" doc,
        ranks = parseRanks doc,
        isRepublish = at "is_republish" doc,
        staffInfo = at "staff" doc
    }

parseRecordingDataEntry :: Document -> Parsed Cvse'RecordingDataEntry
parseRecordingDataEntry doc = Cvse'RecordingDataEntry {
        bvid = at "bvid" doc,
        avid = at "avid" doc,
        view = at "view" doc,
        favorite = at "favorite" doc,
        coin = at "coin" doc,
        like = at "like" doc,
        share = at "share" doc,
        danmaku = at "danmaku" doc,
        reply = at "reply" doc,
        date = buildTime . utcToSystemTime $ at "date" doc
    }

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
        "date" =: (systemToUTCTime . parseSystemTime) entry.date
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
    , join $ filterJust [
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

getAllMetaInfo :: Bool -> Bool -> Selector
getAllMetaInfo get_unexamined get_unincluded =
    join $ filterJust [
        if not get_unexamined
            then Just ["is_examined" =: True]
            else Nothing,
        if not get_unincluded
            -- 只选择至少有一个收录标记的视频
            then Just [
                "$or" =: [
                    inDomesticField,
                    inSVField,
                    inUTAUField
                ]
            ]
            else Nothing
    ]

lookupMetaInfoQuery :: Parsed Cvse'Index -> Selector
lookupMetaInfoQuery index =
    ["_id" =: index.bvid]

lookupDataInfoQuery :: Parsed Cvse'Index -> Parsed Cvse'Time -> Parsed Cvse'Time -> Selector
lookupDataInfoQuery index from_date to_date =
    [ "$and" =: [
        ["bvid" =: index.bvid],
        ["date" =: [ "$gte" =: (systemToUTCTime . parseSystemTime) from_date ]],
        ["date" =: [ "$lte" =: (systemToUTCTime . parseSystemTime) to_date ]]
      ]
    ]
