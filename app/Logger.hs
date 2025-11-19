module Logger(
    safeLiftIO,
    fmtLogger,
    setLoggerLevel,
    outputLoggerToFile,
    outputLoggerToStdout,
    defaultLoggerHandler,
    -- defaultLoggerHandler',
    ErrMessage,
    ExceptionE,
    LogE,
    logError,
    logDebug,
    logInfo,
    logWarning
) where
import Prelude hiding (log)
import Control.Monad.Hefty (Eff, Emb, liftIO, (:>), transform, raiseUnder, Throw, FOEs)
import Control.Monad.Hefty.Except (throw)
import qualified Control.Monad.Hefty.Except as Except
import Control.Monad.Hefty.Log (log, runLogAction, Log)
import Colog.Message (fmtRichMessageCustomDefault, Message(..), Msg(..), showThreadId)
import qualified Data.Time as C
import Colog (
    Severity(..), LogAction(..), logTextHandle, cmapM, fmtMessage, fmtRichMessageDefault, 
    upgradeMessageAction, defaultFieldMap, logTextStdout, showTimeOffset,
    filterBySeverity
    )
import Data.Text (Text, pack, intercalate)
import System.IO (openFile, IOMode(AppendMode), hClose)
import Control.Exception.Base (catch, SomeException)
import GHC.Stack (callStack, withFrozenCallStack)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe (fromMaybe)

type ErrMessage = Text
type ExceptionE = Throw ErrMessage
type LogE = Log Message

codeLocation :: Q Exp
codeLocation = do
  loc <- location
  let file = loc_filename loc
      line = fst (loc_start loc)
  return $ LitE (StringL (file ++ ":" ++ show line))

-- wrap all IO exceptions into ErrMessage
safeLiftIO :: forall es a. (Emb IO :> es, Throw ErrMessage :> es) => IO a -> Eff es a
safeLiftIO c = do
    io_result <- liftIO $ do
        catch (Right <$> c)
            (\(e :: SomeException) -> return $ Left (pack $ show e))
    case io_result of
        Right val -> pure val
        Left err -> throw err
{-# INLINE safeLiftIO #-}

transformLoggerAction :: (LogAction (Eff (Log n ': es)) n -> LogAction (Eff (Log n ': es)) m) 
    -> Eff (Log m ': es) a -> Eff (Log n ': es) a
transformLoggerAction f = runLogAction (f action)  . raiseUnder
    where
        action :: LogAction (Eff (Log n ': es)) n
        action = LogAction {unLogAction = log}
{-# INLINE transformLoggerAction #-}

fmtLogger :: (Emb IO :> es) => Eff (Log Message ': es) a -> Eff (Log Text ': es) a
fmtLogger = transformLoggerAction (upgradeMessageAction defaultFieldMap . cmapM (\rmsg -> fmtRichMessageCustomDefault rmsg (formatter rmsg)))
    where 
        formatter rmsg threadid_maybe time_maybe msg =
            let thread_id = fmap showThreadId threadid_maybe in
            let time_str = fmap (showTimeOffset . C.utcToZonedTime (C.hoursToTimeZone 8)) time_maybe in
            let serve = case msgSeverity msg of
                            Debug -> "[DEBUG] "
                            Info -> "[INFO] "
                            Warning -> "[WARN] "
                            Error -> "[ERROR] " in
            intercalate "" 
                    [ 
                        fromMaybe "" time_str
                        , fromMaybe "" thread_id
                        , serve
                        , msgText msg
                    ]

{-# INLINE fmtLogger #-}

setLoggerLevel :: Severity -> Eff (Log Message ': es) a -> Eff (Log Message ': es) a
-- setLoggerLevel level = runLogAction (filterBySeverity level msgSeverity action) . raiseUnder 
--     where
--         action = LogAction {unLogAction = log}
setLoggerLevel level = transformLoggerAction (filterBySeverity level msgSeverity)
{-# INLINE setLoggerLevel #-}

outputLoggerToFile :: (Emb IO :> es, ExceptionE :> es, FOEs es) => FilePath -> Eff (Log Text ': es) a -> Eff (Except.Catch ErrMessage ': es) a
outputLoggerToFile path f = do
    handle <- safeLiftIO $ openFile path AppendMode
    let clean_up = safeLiftIO $ hClose handle
    result <- Except.catch 
        (runLogAction (logTextHandle handle) (raiseUnder f))
        (\(e :: ErrMessage) -> do
            clean_up
            throw e)
    clean_up
    return result
{-# INLINE outputLoggerToFile #-}

outputLoggerToStdout :: (Emb IO :> es, ExceptionE :> es) => Eff (Log Text ': es) a -> Eff es a
outputLoggerToStdout = runLogAction logTextStdout
{-# INLINE outputLoggerToStdout #-}

defaultLoggerHandler :: (Emb IO :> es, ExceptionE :> es, FOEs es) => FilePath -> Severity -> Eff (Log Message ': es) a -> Eff (Except.Catch ErrMessage : es) a
defaultLoggerHandler fp serve = 
    outputLoggerToFile fp . fmtLogger . setLoggerLevel serve
{-# INLINE defaultLoggerHandler #-}

-- defaultLoggerHandler' :: (Emb IO :> es, ExceptionE :> es, FOEs es) => FilePath -> Severity -> Eff (Log Message ': es) a -> Eff es a
-- defaultLoggerHandler' fp serve = Except.runCatch . (defaultLoggerHandler fp serve)
-- {-# INLINE defaultLoggerHandler' #-}

log1 :: Q Exp
-- log1 sever msg pos = withFrozenCallStack $ log msg1
--     where
--         msg1 = Msg {
--             msgSeverity = sever,
--             msgStack = callStack,
--             msgText = "[" <> pos <> "] " <> msg        
--         }   
log1 = [| \sever msg -> log Msg {
        msgSeverity = sever,
        msgStack = callStack,
        msgText = "[" <> pack $(codeLocation) <> "] " <> msg        
    } |]

{-# INLINE log1 #-}

logError :: Q Exp
logError = [| $(log1) Error |]

{-# INLINE logError #-}
logWarning :: Q Exp
logWarning = [| $(log1) Warning |]
{-# INLINE logWarning #-}
logInfo :: Q Exp
logInfo = [| $(log1) Info |]
{-# INLINE logInfo #-}
logDebug :: Q Exp
logDebug = [| $(log1) Debug |]
{-# INLINE logDebug #-}