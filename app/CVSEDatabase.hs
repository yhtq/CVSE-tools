{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module CVSEDatabase where
import Database.MongoDB
import qualified Database.MongoDB.Admin as DBA
import Logger
import Data.Time (diffUTCTime, UTCTime, LocalTime(..), addLocalTime, addGregorianMonthsClip, addGregorianMonthsRollOver, localTimeToUTC, addUTCTime)
import qualified Data.Time.Clock as TC
import qualified Data.Time.LocalTime as TCL
import Data.Time.Format.ISO8601 (calendarFormat, formatShow, FormatExtension(BasicFormat))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Calendar (fromGregorian, calendarWeek)
import DatabaseProvider
import Data.Text (pack, Text, append)
import Capnp.Gen.CVSEAPI.CVSE
import qualified Capnp as C
import Capnp (Raw, Mutability(..))
import Control.Monad.Hefty (Eff, (:>), Emb)
import Control.Monad (join, void, unless)
import Data.List (nub)
import Data.Time.Clock.System (SystemTime (..), utcToSystemTime, systemToUTCTime)
import Data.Semigroup (stimes)
import qualified Data.Time as TCL
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.String (IsString(..))
import Data.Time.Clock (NominalDiffTime)
import GHC.Base (when)
import Data.Maybe (isNothing, isJust)
import Control.Monad.Hefty.Except (Throw, throw)

addLocalDurationClip :: TCL.CalendarDiffTime -> LocalTime -> LocalTime
addLocalDurationClip (TCL.CalendarDiffTime m d) (LocalTime day t) =
    addLocalTime d $ LocalTime (addGregorianMonthsClip m day) t

addLocalDurationRollOver :: TCL.CalendarDiffTime -> LocalTime -> LocalTime
addLocalDurationRollOver (TCL.CalendarDiffTime m d) (LocalTime day t) =
    addLocalTime d $ LocalTime (addGregorianMonthsRollOver m day) t

uft8 :: TCL.TimeZone
uft8 = TCL.hoursToTimeZone 8

-- 注意参数是第 1 期的结束时间
getRankEndTime :: LocalTime -> Integer -> UTCTime
getRankEndTime firstEnd index =
    let diffDays = TCL.calendarTimeDays $ TCL.CalendarDiffDays 0 ((index - 1) * 7) in
    localTimeToUTC uft8 $ addLocalDurationRollOver diffDays firstEnd

-- SV 刊第 182 期截止时间为 2026/2/14 0.00:00 UTC+8
-- 第 i 期为该时间之后 i - 1 周
svRankEndTime :: Integer -> UTCTime
svRankEndTime index =
    let firstEnd = LocalTime (fromGregorian 2026 2 14) (TCL.TimeOfDay 0 0 0) in
    getRankEndTime firstEnd (index - 181)

-- 第 i 期开始时间为第 i - 1 期截止时间
svRankStartTime :: Integer -> UTCTime
svRankStartTime index = svRankEndTime (index - 1)

-- UTAU 刊第一期为 2026/2/14 0.00:00 UTC+8
utauRankEndTime :: Integer -> UTCTime
utauRankEndTime index =
    let firstEnd = LocalTime (fromGregorian 2026 2 14) (TCL.TimeOfDay 0 0 0) in
    getRankEndTime firstEnd index

-- 第 i 期开始时间为第 i - 1 期截止时间
utauRankStartTime :: Integer -> UTCTime
utauRankStartTime index = utauRankEndTime (index - 1)

-- 国产榜第 68 期结束时间为 2026/2/14 0.00:00 UTC+8
domesticRankEndTime :: Integer -> UTCTime
domesticRankEndTime index =
    let firstEnd = LocalTime (fromGregorian 2026 2 14) (TCL.TimeOfDay 0 0 0) in
    getRankEndTime firstEnd (index - 67)

-- 第 i 期开始时间为第 i - 1 期截止时间
domesticRankStartTime :: Integer -> UTCTime
domesticRankStartTime index = domesticRankEndTime (index - 1)

-- 分别指上主榜和上副榜的最大排名
data RankingConfig = RankingConfig {
    maxMain :: Int,
    maxSide :: Int
}

__rankingConfig :: Cvse'Rank'RankValue -> RankingConfig
__rankingConfig Cvse'Rank'RankValue'domestic = RankingConfig {
        maxMain = 10,
        maxSide = 70
    }
__rankingConfig Cvse'Rank'RankValue'sv = RankingConfig {
        maxMain = 20,
        maxSide = 110
    }
__rankingConfig Cvse'Rank'RankValue'utau = RankingConfig {
        maxMain = 20,
        maxSide = 110
    }
rankingConfig :: Parsed Cvse'Rank -> RankingConfig
rankingConfig rank =
    __rankingConfig rank.value

data EachRankingConfig = EachRankingConfig {
    rank :: Parsed Cvse'Rank,
    index :: Int,
    containUnexamined :: Bool
}

(|-|) :: EachRankingConfig -> Int -> EachRankingConfig
(|-|) config n = let ori_index = config.index in
    EachRankingConfig {rank = config.rank, index = ori_index - n, containUnexamined = config.containUnexamined}

(|+|) :: EachRankingConfig -> Int -> EachRankingConfig
(|+|) config n = let ori_index = config.index in
    EachRankingConfig {rank = config.rank, index = ori_index + n, containUnexamined = config.containUnexamined}

mapCursorBatch :: (MonadIO m) => (Document -> a) -> Cursor -> Action m [a]
mapCursorBatch f cursor = do
        doc <- next cursor
        case doc of
            Nothing -> return []
            Just d -> do
                rest <- mapCursorBatch f cursor
                return (f d : rest)
        -- curs <- nextBatch cursor
        -- if null curs
        --     then return []
        -- else do
        --     rest <- mapCursorBatch f cursor
        --     return (fmap f curs ++ rest)
    -- map f <$> rest cursor

-- 数据库结构：
-- video_metadata_collection:
--   存储视频的基本信息，如视频ID、标题、描述、上传时间等。
--   我们使用 bvid 字符串（包含 “BV” 前缀）作为 _id
--   使用 "examined" 字段表示该视频是否已经被处理过（布尔值）。
-- video_stats_collection:
--   存储视频的统计数据，如播放量、点赞数、评论数等。
--   使用 "date" 记录采集时间（BSON data 类型），自动创建 id 字段。


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
            then Just ["$set" =: genRank entry.ranks]
            else Nothing,
        if entry.hasIsRepublish
            then Just ["$set" =: ["is_republish" =: entry.isRepublish]]
            else Nothing,
        if entry.hasStaffInfo
            then Just ["$set" =: ["staff" =: entry.staffInfo]]
            else Nothing,
        if entry.hasIsExamined
            then Just ["$set" =: ["is_examined" =: entry.isExamined]]
            else Nothing
    ]
    , []
    )

getFromToSelector :: Text -> Parsed Cvse'Time -> Parsed Cvse'Time -> Selector
getFromToSelector dateField from_date to_date =
    [
        dateField =:
            [ "$gte" =: (systemToUTCTime . parseSystemTime) from_date,
               "$lt" =: (systemToUTCTime . parseSystemTime) to_date
            ]
    ]

getAllMetaInfo :: Bool -> Bool -> Parsed Cvse'Time -> Parsed Cvse'Time -> Selector
getAllMetaInfo get_unexamined get_unincluded from_date to_date =
    join (filterJust [
        if not get_unexamined
            then Just ["is_examined" =: True]
            else Nothing,
        if not get_unincluded
            -- 只选择至少有一个收录标记的视频
            then Just [
                "$or" =: [
                    [inDomesticField],
                    [inSVField],
                    [inUTAUField],
                    ["is_examined" =: False]
                ]
            ]
            else Nothing
    ]) ++ getFromToSelector "pubdate" from_date to_date

lookupMetaInfoQuery :: [Parsed Cvse'Index] -> Selector
lookupMetaInfoQuery indices =
    let bvids = map (\index -> index.bvid) indices in
    ["_id" =: ["$in" =: bvids]]


lookupDataInfoQuery :: Parsed Cvse'Index -> Parsed Cvse'Time -> Parsed Cvse'Time -> Selector
lookupDataInfoQuery index from_date to_date =
    ("bvid" =: index.bvid) : getFromToSelector "date" from_date to_date


type Period = (UTCTime, UTCTime)

newtype FieldCalc = FieldCalc Value
    deriving (Show)

unfoldField :: FieldCalc -> Value
unfoldField (FieldCalc f) = f
toExpr :: FieldCalc -> [Value]
toExpr (FieldCalc f) = [f]

instance Num FieldCalc where
    fromInteger n = FieldCalc (val n)
    (FieldCalc f1) + (FieldCalc f2) = FieldCalc $ val ("$add" =: [f1, f2])
    (FieldCalc f1) - (FieldCalc f2) = FieldCalc $ val ("$subtract" =: [f1, f2])
    (FieldCalc f1) * (FieldCalc f2) = FieldCalc $ val ("$multiply" =: [f1, f2])
    negate (FieldCalc f) = FieldCalc $ val ("$multiply" =: [f, val (-1)])
    abs (FieldCalc f) = FieldCalc $ val ("$abs" =: f)
    signum _ = error "signum not implemented for FieldCalc"


instance Fractional FieldCalc where
    fromRational r = FieldCalc (val (fromRational r :: Double))
    (FieldCalc f1) / (FieldCalc f2) = FieldCalc $ val ("$divide" =: [f1, f2])

instance IsString FieldCalc where
    fromString s = FieldCalc (val s)

raw :: Val a => a -> FieldCalc
raw = FieldCalc . val

ltField :: FieldCalc -> FieldCalc -> Field
ltField (FieldCalc f1) (FieldCalc f2) = "$lt" =: [f1, f2]
lteField :: FieldCalc -> FieldCalc -> Field
lteField (FieldCalc f1) (FieldCalc f2) = "$lte" =: [f1, f2]
gtField :: FieldCalc -> FieldCalc -> Field
gtField (FieldCalc f1) (FieldCalc f2) = "$gt" =: [f1, f2]
gteField :: FieldCalc -> FieldCalc -> Field
gteField (FieldCalc f1) (FieldCalc f2) = "$gte" =: [f1, f2]

minField :: FieldCalc -> FieldCalc -> FieldCalc
minField (FieldCalc f1) (FieldCalc f2) = FieldCalc $ val ("$min" =: [f1, f2])

maxField :: FieldCalc -> FieldCalc -> FieldCalc
maxField (FieldCalc f1) (FieldCalc f2) = FieldCalc $ val ("$max" =: [f1, f2])

iteField :: FieldCalc -> FieldCalc -> FieldCalc -> FieldCalc
iteField (FieldCalc cond) (FieldCalc fTrue) (FieldCalc fFalse) =
    FieldCalc $ val ("$cond" =: [cond, fTrue, fFalse])

roundField :: FieldCalc -> Integer -> FieldCalc
roundField (FieldCalc f) digits = FieldCalc $ val ("$round" =: [f, val digits])

roundFixField :: FieldCalc -> FieldCalc
roundFixField f = roundField f 2

sqrtField :: FieldCalc -> FieldCalc
sqrtField (FieldCalc f) = FieldCalc $ val ("$sqrt" =: [f])

-- given f1, f2, f3
-- calculate f1 / f2, if f2 == 0 then f3
safeDivField :: FieldCalc -> FieldCalc -> FieldCalc -> FieldCalc
safeDivField (FieldCalc f1) (FieldCalc f2) defaultField =
    iteField (raw $ eqField (FieldCalc f2) 0)
        defaultField
        (FieldCalc $ val ("$divide" =: [f1, f2]))

eqField :: FieldCalc -> FieldCalc -> Field
eqField (FieldCalc f1) (FieldCalc f2) = "$eq" =: [f1, f2]

clampField :: FieldCalc -> FieldCalc -> FieldCalc -> FieldCalc
clampField f min max =
    let fMin = maxField f min in
    minField fMin max

viewField :: FieldCalc
viewField = "$view"
shareField :: FieldCalc
shareField = "$share"
likeField :: FieldCalc
likeField = "$like"
favoriteField :: FieldCalc
favoriteField = "$favorite"
coinField :: FieldCalc
coinField = "$coin"
replyField :: FieldCalc
replyField = "$reply"
danmakuField :: FieldCalc
danmakuField = "$danmaku"

-- 我们使用的算分公式如下：
-- 基本公式
-- 播放点赞分享得分 + 收藏硬币得分 + 评论弹幕得分←
-- 播放点赞分享得分：
-- 得点A = 播放 + 点赞 + 分享*10
-- 修正A = 得点B * 修正B/得点A(最大值:0.5，最小值：0，投稿两周以内的新曲最小值 = 根号修正A*根号0.5)
-- 播放点赞分享得分 = 得点A * 修正A
-- 收藏硬币得分：
-- 得点B = 2 * 收藏 + 3 * 硬币，且同时不超过收藏数的7倍和硬币数的24倍←
-- 修正B = (得点B/播放) * 25 (最大值:12.5，最小值：0)
-- 收藏硬币得分 = 得点B * 修正B
-- 评论弹幕得分：
-- 得点 C = 评论 + 弹幕
-- 修正C = 得点B / 得点C (最大值:5，最小值：0)
-- 评论弹幕得分 = 得点C * 修正C
-- ：所有修正的小数点四舍五入保留前两位，总分四舍五入至整数←

-- 给定待排名的视频信息 Cursor，记录排行信息的 Collection 名，获得开始时间的 Period 和结束时间的 Period，计算并存储视频得分信息
-- 包括具体的每项得分，每项修正值
-- 对于每个时间段，start 是标准时间（例如零点），end 是采集数据的容差时间（例如 start 时间之后的一小时）
-- 注意，该函数不会处理 drop 和 lock 逻辑
-- 请在外层处理

-- on_main 字段做特殊处理，不会在这里设置，参考下面的 processSHAndHOT 函数
calculateVideoPoints :: [Text] -> Collection -> Period -> Period -> Action IO ()
calculateVideoPoints bvids collection (start1, end1) (start2, end2) = do
    insertAll_ collection (map
        (\bvid -> ["_id" =: bvid])
        bvids
        )
    let twoWeeksAgo = addUTCTime (negate $ realToFrac (14 * 24 * 60 * 60)) start2
        lookupMetaInfoStage =
            [ "$lookup" =: [
                "from" =: videoMetadataCollection,
                "localField" =: "_id",
                "foreignField" =: "_id",
                "as" =: "metas"
            ] ]
        newFieldStage = [
            "$addFields" =: [
                "meta" =: [ "$arrayElemAt" =: [val "$metas", val 0] ]
            ] ]
        lookupAs (start, end) asName = [
            "$lookup" =: [
                "from" =: videoStatsCollection,
                "let" =: ["bvid" =: "$_id"],
                "pipeline" =: [
                    ["$match" =: [
                        "$expr" =: [
                            "$and" =: [
                                ["$bvid" `eqField` "$$bvid"],
                                ["$date" `gteField` raw start],
                                ["$date" `ltField` raw end]
                            ]
                        ]
                    ]]
                ],
                "as" =: asName
              ]
            ]
        lookupPrevStage = lookupAs (start1, end1) "prev_stats"
        lookupCurrStage = lookupAs (start2, end2) "curr_stats"
        newFieldStage1 =
            [ "$addFields" =: [
                "isNew" =: ["$and" =: [
                    ["$meta.pubdate" `gteField` raw start1],
                    ["$meta.pubdate" `ltField` raw start2]
                ]],
                "avid" =: "$meta.avid"
            ] ]
        -- 只处理结束周期内有数据，开始周期内有数据或者是新曲的数据
        matchWithDataStage =
            [ "$match" =: [
                "$expr" =: [
                    "$or" =: [
                        ["$and" =: [
                            [raw ("$size" =: "$curr_stats") `gtField` (raw 0)],
                            [raw ("$size" =: "$prev_stats") `gtField` (raw 0)]
                        ]],
                        ["$and" =: [
                            [raw ("$size" =: "$curr_stats") `gtField` raw 0],
                            ["$isNew" `eqField` raw True]
                        ]]
                    ]
                ]
            ] ]
        -- 任选 match 结果作为 prev 和 curr 数据
        addPeriodStage =
            [ "$addFields" =: [
                "prev" =: [
                    -- 如果是新曲，采取全零数据
                    "$cond" =: [
                        val $ "$isNew" `eqField` raw True,
                        val [
                            "avid" =: "av1",
                            "bvid" =: "BV1",
                            "view" =: 0,
                            "like" =: 0,
                            "share" =: 0,
                            "favorite" =: 0,
                            "coin" =: 0,
                            "reply" =: 0,
                            "danmaku" =: 0,
                            "date" =: (systemToUTCTime $ MkSystemTime 0 0)
                        ],
                        val ("$arrayElemAt" =: [val "$prev_stats", val 0])
                    ]
                ],
                "curr" =: [ "$arrayElemAt" =: [val "$curr_stats", val 0] ]
            ] ]
        diffStage =
            [ "$addFields" =: [
                "view" =: unfoldField ("$curr.view" - "$prev.view"),
                "like" =: unfoldField ("$curr.like" - "$prev.like"),
                "share" =: unfoldField ("$curr.share" - "$prev.share"),
                "favorite" =: unfoldField ("$curr.favorite" - "$prev.favorite"),
                "coin" =: unfoldField ("$curr.coin" - "$prev.coin"),
                "reply" =: unfoldField ("$curr.reply" - "$prev.reply"),
                "danmaku" =: unfoldField ("$curr.danmaku" - "$prev.danmaku")
            ] ]
        pointsStage =
            [ "$addFields" =: [
                "pointA" =: unfoldField (viewField + likeField + 10 * shareField),
                "pointB" =:
                    let ori = 2 * favoriteField + 3 * coinField in
                    let clamped = minField (7 * favoriteField) (24 * coinField) in
                    unfoldField $ minField ori clamped,
                "pointC" =: unfoldField (replyField + danmakuField)
            ] ]
        fixBStage = [
            "$addFields" =: [
                "fixB" =:
                    let fixBOri = safeDivField (25 * "$pointB") viewField 12.5 in
                    let clamped = clampField fixBOri 0 12.5 in
                    unfoldField $ roundFixField clamped
            ] ]
        fixCStage =
            [ "$addFields" =: [
                "fixC" =:
                    let fixCOri = safeDivField "$pointB" "$pointC" 5 in
                    let clamped = clampField fixCOri 0 5 in
                    unfoldField $ roundFixField clamped
            ] ]
        fixAStage =
            [ "$addFields" =: [
                "fixA" =:
                    let baseFixA = safeDivField "$pointB" "$pointA" 0.5 in
                    let clampedFixA = clampField baseFixA 0 0.5 in
                    let newMinFixA = sqrtField clampedFixA * sqrtField 0.5 in
                    let pubdateInTwoWeeks = raw ["$gte" =: [val "$meta.pubdate", val twoWeeksAgo]] in
                    let result = iteField
                            pubdateInTwoWeeks
                            (clampField baseFixA newMinFixA 0.5)
                            clampedFixA
                    in
                    unfoldField $ roundFixField result
            ] ]
        scoreStage =
            [ "$addFields" =: [
                "scoreA" =: unfoldField ("$pointA" * "$fixA"),
                "scoreB" =: unfoldField ("$pointB" * "$fixB"),
                "scoreC" =: unfoldField ("$pointC" * "$fixC")
            ] ]
        totalStage =
            [ "$addFields" =: [
                "totalScore" =: unfoldField (roundField ("$scoreA" + "$scoreB" + "$scoreC") 0),
                specialRankField =: normalContent   -- 没有处理特殊排名规则，全部设为普通排名
            ] ]
        cleanStage = [
            "$project" =: [
                "metas" =: 0,
                "meta" =: 0,
                "prev_stats" =: 0,
                "curr_stats" =: 0
            ]
            ]
        cleanStage2 = [
            "$addFields" =: [
                -- curr field 只留数据的 id
                "curr_data_id" =: "$curr._id",
                -- 如果不是新曲，prev field 只留数据的 id，否则设为 null
                "prev_data_id" =: unfoldField (iteField
                        (raw $ "$isNew" `eqField` raw True)
                        (raw Null)
                        (raw ["_id" =: "$prev._id"]))
            ]]
        cleanStage3 = [
            "$project" =: [
                "curr" =: 0,
                "prev" =: 0
            ]
            ]
        -- 按分数排名并计入排序，同分的同排名
        sortStage = [
            "$setWindowFields" =: [
                "sortBy" =: ["totalScore" =: -1],
                "output" =: [
                    "rank" =: [
                        "$rank" =: ([] :: Document) -- we can not write $rank := [] here, otherwise it will be treated as empty int array
                    ]
                ]
            ] ]
        outStage = [
            "$out" =: collection
            ]

        statsPipeline = [
            lookupPrevStage,
            lookupCurrStage,
            lookupMetaInfoStage,
            newFieldStage,
            newFieldStage1,
            matchWithDataStage,
            addPeriodStage,
            diffStage,
            pointsStage,
            fixBStage,
            fixCStage,
            fixAStage,
            scoreStage,
            totalStage,
            sortStage,
            cleanStage,
            cleanStage2,
            cleanStage3,
            outStage
            ]
        -- 接下来，计算该期排行榜的统计数据
        statsPipeline2 = [
            [
                "$group" =: [
                    "_id" =: Null,
                    "count" =: ["$sum" =: 1],
                    "totalNew" =: ["$sum" =: ["$cond" =: [val "$isNew", val 1, val 0]]],
                    "totalView" =: ["$sum" =: "$view"],
                    "totalLike" =: ["$sum" =: "$like"],
                    "totalShare" =: ["$sum" =: "$share"],
                    "totalFavorite" =: ["$sum" =: "$favorite"],
                    "totalCoin" =: ["$sum" =: "$coin"],
                    "totalReply" =: ["$sum" =: "$reply"],
                    "totalDanmaku" =: ["$sum" =: "$danmaku"]
                ]
            ],
            [
                "$addFields" =: [
                    "_id" =: collection,
                    "startTime" =: start1,
                    "endTime" =: start2
                ]
            ],
            [
                "$merge" =: [
                    "into" =: rankingMetaInfoCollectionName,
                    "whenMatched" =: "replace",
                    "whenNotMatched" =: "insert"
                ]
            ] ]
    do
        void $ aggregateCursor collection statsPipeline (AggregateConfig {allowDiskUse = True}) >>= rest
        void $ aggregateCursor collection statsPipeline2 (AggregateConfig {allowDiskUse = True}) >>= rest

rankingMetaInfoCollectionName :: Text
rankingMetaInfoCollectionName = "ranking_meta_info"

checkLock :: Collection -> Action IO Bool
checkLock collection = do
    mdoc <- findOne (select ["_id" =: collection, "locked" =: True] rankingMetaInfoCollectionName)
    case mdoc of
        Just _ -> return True
        Nothing -> return False

lockCollection :: Collection -> Action IO ()
lockCollection collection = do
    void $ upsert (select ["_id" =: collection] rankingMetaInfoCollectionName)
        ["$set" =: ["locked" =: True]]

unlockCollection :: Collection -> Action IO ()
unlockCollection collection = do
    void $ upsert (select ["_id" =: collection] rankingMetaInfoCollectionName)
        ["$set" =: ["locked" =: False]]

rankStr :: Parsed Cvse'Rank -> String
rankStr rank = case rank.value of
    Cvse'Rank'RankValue'domestic -> "domestic"
    Cvse'Rank'RankValue'sv -> "sv"
    Cvse'Rank'RankValue'utau -> "utau"

genCollectionName :: EachRankingConfig -> Collection
genCollectionName (EachRankingConfig {rank=rank, index=index, containUnexamined=contain_unexamined}) =
    let examStr = if contain_unexamined then "_with_unexamined" else ""
    in
    pack $ "video_points_" ++ rankStr rank ++ "_rank_" ++ show index ++ examStr

genSHCountingCollectionName :: EachRankingConfig -> Collection
genSHCountingCollectionName (EachRankingConfig {rank=rank, index=index, containUnexamined=contain_unexamined}) =
    let examStr = if contain_unexamined then "_with_unexamined" else ""
    in
    pack $ "sh_counting_" ++ rankStr rank ++ "_rank_" ++ show index ++ examStr

genRecentTenWeeksOnMainCountingCollectionName :: EachRankingConfig -> Collection
genRecentTenWeeksOnMainCountingCollectionName (EachRankingConfig {rank=rank, index=index, containUnexamined=contain_unexamined}) =
    let examStr = if contain_unexamined then "_with_unexamined" else ""
    in
    pack $ "recent_ten_weeks_on_main_counting_" ++ rankStr rank ++ "_rank_" ++ show index ++ examStr

specialRankField :: Text
specialRankField = "special_rank"

shContent :: Text
shContent = "SH"

hotContent :: Text
hotContent = "HOT"

normalContent :: Text
normalContent = "NORMAL"

lookupUsingIdWithDefault :: Collection -> [(Text, Text, Value)] -> [Document]
lookupUsingIdWithDefault collection lookups = [
        "$lookup" =: [
            "from" =: collection,
            "localField" =: "_id",
            "foreignField" =: "_id",
            "as" =: "___lookup_result___"
        ]
    ] : map
    (\(foreignField, asField, defaultValue) ->
        [
            "$addFields" =: [
                asField =: [
                    "$cond" =: [
                        val ["$gt" =: [val ["$size" =: "$___lookup_result___"], val 0]],
                        val ["$arrayElemAt" =: [val ("$___lookup_result___." <> foreignField), val 0]],
                        defaultValue
                    ]
                ]
            ]
        ]
    ) lookups ++ [
        [
            "$project" =: [
                "___lookup_result___" =: 0
            ]
        ]]

isHot = raw ("$" <> specialRankField) `eqField` raw hotContent
isSH = raw ("$" <> specialRankField) `eqField` raw shContent

-- 假设进入该函数时，状态是刚刚完成 calculateVideoPoints 的状态
-- 结束后，会将 ranking collection 更新至最终状态，并生成 SH 和 HOT 统计集合
processSHAndHot :: forall es. (LogE :> es, Emb IO :> es, Throw ErrMessage :> es, DataBaseTableState :> es) =>
    EachRankingConfig -> Eff (DatabaseIO ': es) ()
processSHAndHot config@(EachRankingConfig {rank=rank, index=index, containUnexamined=contain_unexamined}) = do
    let currRankingInfoCollection = genCollectionName config
        currSHCollection = genSHCountingCollectionName config
        currHOTCollection = genRecentTenWeeksOnMainCountingCollectionName config
        prevSHCollection = genSHCountingCollectionName (config |-| 1)
        prevHOTCollection = genRecentTenWeeksOnMainCountingCollectionName (config |-| 1)
        -- HOT 统计是统计近十周的上主榜次数 (curr |-| 0 到 curr |-| 9)，因此只要在上周的 HOT 统计基础上，减去 curr |-| 10，再加上 curr |-| 0 即可
        tenWeeksAgoRankingInfoCollection = genCollectionName (config |-| 10)
    -- drop 掉当前的 SH 和 HOT 统计集合
    runDbAction $ do 
        dropCollection currSHCollection
        dropCollection currHOTCollection
        createIndex (DBA.index currSHCollection ["rank" =: (-1), "totalScore" =: (-1)])
    -- 生成初始的 currSHCollection：对于 currRankingInfoCollection 中 rank <= 3 的视频，计数是上期的计数加一，否则延续上期计数（如果上期不存在，则计数是 0）
    prevSHExists <- runDbAction $ checkCollectionExists prevSHCollection
    prevHOTExists <- runDbAction $ checkCollectionExists prevHOTCollection
    tenWeeksAgoExists <- runDbAction $ checkCollectionExists tenWeeksAgoRankingInfoCollection
    assert (prevSHExists == prevHOTExists) "SH and HOT previous collection existence mismatch"

    let mergePrevSHPipe = lookupUsingIdWithDefault prevSHCollection
            [("count", "prev_count", val 0)]
    -- 由于 HOT 的特殊性，统计前三/主榜时，应当使用的方法是分数大于等于第三名/主榜最后一名的分数
    let get_least_score_rank_gte n = do
            doc <- runDbAction $ aggregateCursor currRankingInfoCollection [
                    [
                        "$match" =: [
                            "$expr" =: [
                                -- 例如如果两个第二名同分，1 2 2 4 排名，则条件就是分数大于等于第二名的分数
                                lteField (raw "$rank") (raw n)
                            ]
                        ]
                    ],
                    [
                        -- rank 从大到小排名
                        "$sort" =: ["rank" =: -1]
                    ],
                    [
                        "$limit" =: 1
                    ]
                    ] (AggregateConfig {allowDiskUse = True}) >>= rest
            case doc of
                [] -> throw $ "No video found with rank greater than " <> pack (show n)
                (d:_) -> return (at "totalScore" d :: Float)
    let maxMain = (rankingConfig config.rank).maxMain
    let in_first_three = do
            leastScore <- get_least_score_rank_gte (3 :: Int)
            ($logDebug) ("Least score for top 3: " <> pack (show leastScore))
            return $ "$totalScore" `gteField` raw leastScore
    let on_main = do
            leastScore <- get_least_score_rank_gte maxMain
            ($logDebug) ("Least score for main list: " <> pack (show leastScore))
            return $ "$totalScore" `gteField` raw leastScore
    let resortingPipe temporary = [
            -- 对所有 rank > 0，totalScore > 0 的视频重新排序（减少计算量）
            [
                "$match" =: [
                    "$expr" =: [
                        "$and" =: [
                            [raw "$rank" `gtField` raw 0]
                        ] ++ [[raw "$totalScore" `gtField` raw 0] | temporary]
                    ]
                ]
            ],
            [
                "$setWindowFields" =: [
                    "sortBy" =: ["totalScore" =: -1],
                    "output" =: [
                        "rank" =: [
                            "$rank" =: ([] :: Document)
                        ]
                    ]
                ]
            ],
            [
                "$merge" =: [
                    "into" =: currRankingInfoCollection,
                    "whenMatched" =: [[
                        "$set" =: [
                            "rank" =: "$$new.rank"
                        ]
                    ]],
                    "whenNotMatched" =: "discard"
                ]
            ]]
    let cleanA = modify (select [] currRankingInfoCollection) 
                    [
                        "$unset" =: [
                            "is_sh" =: 1,
                            "old_sh" =: 1,
                            "is_hot" =: 1,
                            "old_hot" =: 1,
                            "sh_status_has_changed" =: 1,
                            "hot_status_has_changed" =: 1
                        ]
                    ]
    when (prevSHExists && prevHOTExists) do
        runDbAction cleanA
        -- 计算本期的所有 HOT 和 SH
        -- 先处理 SH，找到 currRankingInfoCollection 中的前三，如果其中某个视频在 prevSHCollection 中的计数已经 >= 2，则将 rank 修改为 0，special_rank 修改为 shContent
        -- 由于管道需要迭代，因此还要记录 old_sh 字段，也就是如果之前 special_rank 是 shContent，则 old_sh 为 True，否则为 False
        let shPipe in_first_three_cond = [
                [
                    "$match" =: [
                        "$expr" =: [in_first_three_cond]
                    ]
                ]] ++ mergePrevSHPipe ++ [
                [
                    "$project" =: [
                        "_id" =: 1,
                        "is_sh" =: [
                            "$cond" =: [
                                val ["$gte" =: [val "$prev_count", val 2]],
                                val True,
                                val False
                            ]
                        ],
                        "old_sh" =: unfoldField (iteField
                            (raw isSH)
                            (raw True)
                            (raw False)
                        )
                    ]
                ],
                [
                    "$merge" =: [
                        "into" =: currRankingInfoCollection,
                        "whenMatched" =: [[
                            "$set" =: [
                                "old_sh" =: "$$new.old_sh",
                                "is_sh" =: "$$new.is_sh",
                                "rank" =: [
                                    "$cond" =: [
                                        val "$$new.is_sh",
                                        val 0,
                                        val "$rank"
                                    ]
                                ],
                                specialRankField =: [
                                    "$cond" =: [
                                        val "$$new.is_sh",
                                        val shContent,
                                        val ("$" <> specialRankField)
                                    ]
                                ],
                                "sh_status_has_changed" =: unfoldField (iteField
                                    (raw ("$$new.old_sh" `eqField` "$$new.is_sh"))
                                    (raw False)
                                    (raw True))
                            ]
                        ]],
                        "whenNotMatched" =: "discard"
                    ]
                ]
                ]
        -- HOT 的处理方式是：
        -- 首先准备当前的 HOT 计数，具体来说：
        --   当前视频的最近十期主榜计数（HOTCollection 中的 count 字段）= 
        --    上期的 HOT 计数 
        --    - (1 如果 tenWeeksAgoRankingInfoCollection 中存在该视频 on_main = True))
        --    + (1 如果 currRankingInfoCollection 中 rank <= maxMain)
        --   当前视频的 HOT 状态（is_hot 字段）= 不是 SH 且
        --    (上期的 is_hot 字段 && 当前 HOT 计数 > 3)
        --    || (当前 HOT 计数 >= 7)
        -- 之后更新数据，所有当期 is_hot 为 True 的视频，rank 设为 0，special_rank 设置为 hotContent
        let hotPipe on_main_cond =
                lookupUsingIdWithDefault prevHOTCollection
                    [("count", "prev_count", val 0), ("is_hot", "prev_is_hot", val False)]
                ++
                lookupUsingIdWithDefault tenWeeksAgoRankingInfoCollection
                    [("on_main", "ten_weeks_ago_on_main", val False)]
                ++ [
                [
                    "$addFields" =: [
                        "ten_weeks_ago_on_main_diff" =: unfoldField (iteField
                                (raw ("$ten_weeks_ago_on_main" `eqField` raw True))
                                (raw 1)
                                (raw 0)),
                        "on_main_diff" =: unfoldField (iteField
                                (raw on_main_cond)
                                (raw 1)
                                (raw 0)
                        ),
                        "count" =: unfoldField (raw "$prev_count" - raw "$ten_weeks_ago_on_main_diff" + raw "$on_main_diff")
                        ,
                        "old_hot" =: unfoldField (iteField
                                (raw isHot)
                                (raw True)
                                (raw False)
                        ),
                        "is_hot" =: [
                            "$and" =: [
                                [
                                    "$not" =: [
                                        isSH
                                    ]
                                ],
                                [
                                "$or" =: [
                                    [
                                        "$and" =: [
                                            ["$prev_is_hot" `eqField` raw True],
                                            [raw "$count" `gtField` raw 3]
                                        ]
                                    ],
                                    [raw "$count" `gteField` raw 7]
                                ]]
                            ]
                        ]
                    ]
                ],
                [
                    "$merge" =: [
                        "into" =: currRankingInfoCollection,
                        "whenMatched" =: [[
                            "$set" =: [
                                "is_hot" =: "$$new.is_hot",
                                "old_hot" =: "$$new.old_hot",
                                "rank" =: [
                                    "$cond" =: [
                                        val "$$new.is_hot",
                                        val 0,
                                        val "$rank"
                                    ]
                                ],
                                specialRankField =: [
                                    "$cond" =: [
                                        val "$$new.is_hot",
                                        val hotContent,
                                        val ("$" <> specialRankField)
                                    ]
                                ],
                                "hot_status_has_changed" =: unfoldField (iteField
                                    (raw ("$$new.old_hot" `eqField` raw "$$new.is_hot"))
                                    (raw False)
                                    (raw True))
                            ]
                        ]],
                        "whenNotMatched" =: "discard"
                    ]
                ]
                ]
        let processingAux = do
                in_first_three_cond <- in_first_three
                on_main_cond <- on_main
                -- 处理 SH
                shReports <- runDbAction $ aggregateCursor currRankingInfoCollection (shPipe in_first_three_cond) (AggregateConfig {allowDiskUse = True}) >>= rest
                -- 处理 HOT
                hotReports <- runDbAction $ aggregateCursor currRankingInfoCollection (hotPipe on_main_cond) (AggregateConfig {allowDiskUse = True}) >>= rest
                unless (null shReports) $
                    $(logError) $ "SH processing report: " <> pack (show shReports)
                unless (null hotReports) $
                    $(logError) $ "HOT processing report: " <> pack (show hotReports)
                newSH <- runDbAction $ find (select ["sh_status_has_changed" =: True] currRankingInfoCollection) >>= rest
                newHOT <- runDbAction $ find (select ["hot_status_has_changed" =: True] currRankingInfoCollection) >>= rest
                $(logInfo) $ "Videos with SH status changed: " <> pack (show newSH)
                $(logInfo) $ "Videos with HOT status changed: " <> pack (show newHOT)
                -- 如果此轮没有变化，则结束，否则重复进行
                unless (null newSH && null newHOT) do
                    -- 重新排序
                    void $ runDbAction $ aggregateCursor currRankingInfoCollection (resortingPipe True) (AggregateConfig {allowDiskUse = True}) >>= rest
                    processingAux
        processingAux
    -- SH 和 HOT 状态稳定后，进行最终的排序，并清理临时字段
    runDbAction $ do 
        aggregateCursor currRankingInfoCollection 
            (resortingPipe False) 
            (AggregateConfig {allowDiskUse = True}) >>= rest
        cleanA
    
    -- 最后，更新 currSHCollection 和 currHOTCollection
    in_first_three_cond <- in_first_three
    on_main_cond <- on_main
    let shPrecPipe = [
            -- 把 prevSHCollection 中所有视频导入 currSHCollection，作为基础
            [
                "$match" =: ["_id" =: ["$exists" =: True]]
            ],
            [
                "$out" =: currSHCollection
            ]]
    shPrecReport <- runDbAction $ aggregateCursor prevSHCollection shPrecPipe (AggregateConfig {allowDiskUse = True}) >>= rest
    unless (null shPrecReport) $
        $(logError) $ "SH precopy report: " <> pack (show shPrecReport)
    -- 再将 currRankingInfoCollection 中满足前三条件的视频计数加一
    let shCurPipe = [
            [
                "$match" =: [
                    "$expr" =: [in_first_three_cond]
                ]
            ],
            [
                "$project" =: [
                    "_id" =: 1
                ]
            ]] ++
            lookupUsingIdWithDefault currSHCollection
                [("count", "prev_count", val 0)] ++
            [
            [
                "$project" =: [
                    "_id" =: 1,
                    "count" =: unfoldField (raw "$prev_count" + raw 1)
                ]
            ],
            [
                "$merge" =: [
                    "into" =: currSHCollection,
                    "whenMatched" =: "replace",
                    "whenNotMatched" =: "insert"
                ]
            ]]
    shCurReport <- runDbAction $ aggregateCursor currRankingInfoCollection shCurPipe (AggregateConfig {allowDiskUse = True}) >>= rest
    unless (null shCurReport) $
        $(logError) $ "SH current report: " <> pack (show shCurReport)
    -- 更新 currRankingInfoCollection 中的 on_main, on_side 字段
    on_side_cond <- do
            leastScore <- get_least_score_rank_gte (rankingConfig config.rank).maxSide
            return ["$totalScore" `gteField` raw leastScore]
    let finalUpdatePipe = [
            [
                "$addFields" =: [
                    "on_main" =: unfoldField (iteField
                        (raw on_main_cond)
                        (raw True)
                        (raw False)),
                    "on_side" =: unfoldField (iteField
                        (raw on_side_cond)
                        (raw True)
                        (raw False)
                        )
                    ]
            ],
            [
                "$merge" =: [
                    "into" =: currRankingInfoCollection,
                    "whenMatched" =: [[
                        "$set" =: [
                            "on_main" =: "$$new.on_main",
                            "on_side" =: "$$new.on_side"
                        ]
                    ]],
                    "whenNotMatched" =: "discard"
                ]
            ]]
    $(logDebug) $ "Final on_main/on_side update pipe: " <> pack (show finalUpdatePipe)
    updateReport <- runDbAction $ aggregateCursor currRankingInfoCollection finalUpdatePipe (AggregateConfig {allowDiskUse = True}) >>= rest
    $(logDebug) $ "Final on_main/on_side update report: " <> pack (show updateReport)
    -- 以及更新 HOT 计数
    let hotPrecPipe = [
            -- 把 prevHOTCollection 中所有视频导入 currHOTCollection，作为基础
            [
                "$match" =: ["_id" =: ["$exists" =: True]]
            ],
            [
                "$out" =: currHOTCollection
            ]]
    hotPrecReport <- runDbAction $ aggregateCursor prevHOTCollection hotPrecPipe (AggregateConfig {allowDiskUse = True}) >>= rest
    unless (null hotPrecReport) $
        $(logError) $ "HOT precopy report: " <> pack (show hotPrecReport)
    -- 把上主榜的曲子信息加入
    let hotCurrPipe = [
            [
                "$match" =: [
                    "on_main" =: True 
                ]
            ],
            [
                "$addFields" =: [
                    "count" =: val 1
                ]
            ],
            [
                "$project" =: [
                    "_id" =: 1,
                    "is_hot" =: 1,
                    "count" =: 1
                ]
            ],
            [
                "$merge" =: [
                    "into" =: currHOTCollection,
                    "whenMatched" =: [[
                        "$set" =: [
                            "is_hot" =: "$$new.is_hot",
                            "count" =: unfoldField (raw "$count" + 1)
                        ]
                    ]],
                    "whenNotMatched" =: "insert"
                ]
            ]
            ]
    -- let hotCurPipe =
    --         [
    --             "$match" =: [
    --                 "on_main" =: True
    --             ]
    --         ]:
    --         lookupUsingIdWithDefault tenWeeksAgoRankingInfoCollection
    --             [("on_main", "ten_weeks_ago_on_main", val False)]
    --         ++
    --         lookupUsingIdWithDefault prevHOTCollection
    --             [("count", "prev_count", val 0)] 
    --         ++
    --         [
    --             [
    --                 "$addFields" =: [
    --                     "ten_weeks_diff" =: [
    --                         "$cond" =: [
    --                             val ("$ten_weeks_ago_on_main" `eqField` raw True),
    --                             val (-1),
    --                             val 0
    --                         ]
    --                     ],
    --                     "curr_diff" =: [
    --                         "$cond" =: [
    --                             val ("$on_main_curr" `eqField` raw True),
    --                             val 1,
    --                             val 0
    --                         ]
    --                     ],
    --                     "count" =: unfoldField (raw "$ten_weeks_diff" + raw "$curr_diff" + raw "$prev_count")
                        
    --                 ]
    --             ],
    --             [
    --                 "$project" =: [
    --                     "_id" =: 1,
    --                     "count" =: 1,
    --                     "is_hot" =: 1
    --                 ]
    --             ],
    --             [
    --                 "$merge" =: [
    --                     "into" =: currHOTCollection,
    --                     "whenMatched" =: "replace",
    --                     "whenNotMatched" =: "insert"
    --                 ]
    --             ]]
    hotCurrReport <- runDbAction $ aggregateCursor currRankingInfoCollection hotCurrPipe (AggregateConfig {allowDiskUse = True}) >>= rest
    unless (null hotCurrReport) $
        $(logError) $ "HOT current report: " <> pack (show hotCurrReport)
    -- 处理十周前计数
    let hotTenWeeksAgoPipe = [
            [
                "$match" =: [
                    "on_main" =: True
                ]
            ],
            [
                "$project" =: [
                    "_id" =: 1
                ]
            ],
            [
                "$merge" =: [
                    "into" =: tenWeeksAgoRankingInfoCollection,
                    "whenMatched" =: [
                        [
                            "$set" =: [
                                "count" =: unfoldField (raw "$count" - 1)
                            ]
                        ]
                    ],
                    "whenNotMatched" =: "discard"
                ]
            ]
            ]
    when tenWeeksAgoExists do
        hotTenWeeksAgoReport <- runDbAction $ aggregateCursor currRankingInfoCollection hotTenWeeksAgoPipe (AggregateConfig {allowDiskUse = True}) >>= rest
        unless (null hotTenWeeksAgoReport) $
            $(logError) $ "HOT ten weeks ago report: " <> pack (show hotTenWeeksAgoReport)    


genRankingQuery :: EachRankingConfig -> Selector
genRankingQuery (EachRankingConfig {rank=rank, index=index, containUnexamined=contain_unexamined}) =
    let rankField = case rank.value of
            Cvse'Rank'RankValue'domestic -> inDomesticField
            Cvse'Rank'RankValue'sv -> inSVField
            Cvse'Rank'RankValue'utau -> inUTAUField
        examField = if not contain_unexamined
            then Just ["is_examined" =: True]
            else Nothing
    in
    join (filterJust [
        Just [rankField],
        examField
    ])

recordDiffTime :: NominalDiffTime
recordDiffTime = realToFrac (120 * 60) -- 120 minutes

genPeriod :: Parsed Cvse'Rank -> Int -> (Period, Period)
genPeriod rank index =
    let (start1, start2) = case rank.value of
            Cvse'Rank'RankValue'domestic -> (domesticRankStartTime (fromIntegral index), domesticRankEndTime (fromIntegral index))
            Cvse'Rank'RankValue'sv -> (svRankStartTime (fromIntegral index), svRankEndTime (fromIntegral index))
            Cvse'Rank'RankValue'utau -> (utauRankStartTime (fromIntegral index), utauRankEndTime (fromIntegral index))
        end1 = addUTCTime recordDiffTime start1
        end2 = addUTCTime recordDiffTime start2
    in
    ((start1, end1), (start2, end2))

genGetRankInfoQuery :: Int -> Int -> Collection -> Query
genGetRankInfoQuery from_rank to_rank =
    select ["rank" =: ["$gte" =: fromIntegral from_rank, "$lt" =: fromIntegral to_rank]]

getRankInfo :: Int -> Int -> Collection -> Action IO Cursor
getRankInfo from_rank to_rank collection =
    let pipeline = [
            [
                "$match" =: [
                    "rank" =: [
                        "$gte" =: fromIntegral from_rank,
                        "$lt" =: fromIntegral to_rank
                    ]
                ]
            ],
            [
                "$project" =: [
                    "_id" =: 1,
                    "avid" =: 1
                ]
            ]
            ] in
    aggregateCursor collection pipeline (AggregateConfig {allowDiskUse = True})

lookupRankingInfoQuery :: [Parsed Cvse'Index] -> Selector
lookupRankingInfoQuery indices =
    let bvids = map (\index -> index.bvid) indices in
    ["_id" =: ["$in" =: bvids]]

parseRankingInfoEntry :: Document -> Parsed Cvse'RankingInfoEntry
parseRankingInfoEntry doc = Cvse'RankingInfoEntry {
            avid = at "avid" doc,
            bvid = at "_id" doc,
            isNew = at "isNew" doc,
            rank = at "rank" doc,
            curr = parseRecordingDataEntry $ at "curr" doc,
            prev = parseRecordingDataEntry $ at "prev" doc,
            view = at "view" doc,
            like = at "like" doc,
            share = at "share" doc,
            favorite = at "favorite" doc,
            coin = at "coin" doc,
            reply = at "reply" doc,
            danmaku = at "danmaku" doc,
            pointA = at "pointA" doc,
            pointB = at "pointB" doc,
            pointC = at "pointC" doc,
            fixA = at "fixA" doc,
            fixB = at "fixB" doc,
            fixC = at "fixC" doc,
            scoreA = at "scoreA" doc,
            scoreB = at "scoreB" doc,
            scoreC = at "scoreC" doc,
            totalScore = at "totalScore" doc
        }

lookupRankingMetaInfoQuery :: Parsed Cvse'Rank -> Int -> Bool -> Query
lookupRankingMetaInfoQuery rank index containUnexamined =
    let collection = genCollectionName (EachRankingConfig {rank, index, containUnexamined}) in
    select [
        "_id" =: collection
    ] rankingMetaInfoCollectionName

parseRankingMetaInfoStat :: Document -> Parsed Cvse'RankingMetaInfoStat
parseRankingMetaInfoStat doc = Cvse'RankingMetaInfoStat {
    count = fromIntegral $ at "count" doc,
    totalNew = at "totalNew" doc,
    totalView = at "totalView" doc,
    totalLike = at "totalLike" doc,
    totalShare = at "totalShare" doc,
    totalFavorite = at "totalFavorite" doc,
    totalCoin = at "totalCoin" doc,
    totalReply = at "totalReply" doc,
    totalDanmaku = at "totalDanmaku" doc,
    startTime = buildTime . utcToSystemTime $ at "startTime" doc,
    endTime = buildTime . utcToSystemTime $ at "endTime" doc
}