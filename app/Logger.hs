module Logger(
    safeLiftIO,
    fmtLogger,
    setLoggerLevel,
    outputLoggerToFile,
    outputLoggerToStdout,
    defaultLoggerHandler,
    ErrMessage,
    ExceptionE,
    LogE,
    logError,
    logDebug,
    logInfo,
    logWarning
) where
import Prelude hiding (log)
import Control.Monad.Hefty (Eff, Emb, liftIO, (:>), transform, raiseUnder, Throw)
import Control.Monad.Hefty.Except (throw)
import Control.Monad.Hefty.Log (log, runLogAction, Log)
import Colog.Message (fmtRichMessageCustomDefault, Message(..), Msg(..))
import Colog (
    Severity(..), LogAction(..), logTextHandle, cmapM, fmtMessage, fmtRichMessageDefault, 
    upgradeMessageAction, defaultFieldMap, logTextStdout,
    filterBySeverity
    )
import Data.Text (Text, pack)
import System.IO (openFile, IOMode(AppendMode))
import Control.Exception.Base (catch, SomeException)
import GHC.Stack (callStack, withFrozenCallStack)

type ErrMessage = Text
type ExceptionE = Throw ErrMessage
type LogE = Log Message

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
fmtLogger = transformLoggerAction (upgradeMessageAction defaultFieldMap . cmapM fmtRichMessageDefault)
{-# INLINE fmtLogger #-}

setLoggerLevel :: Severity -> Eff (Log Message ': es) a -> Eff (Log Message ': es) a
-- setLoggerLevel level = runLogAction (filterBySeverity level msgSeverity action) . raiseUnder 
--     where
--         action = LogAction {unLogAction = log}
setLoggerLevel level = transformLoggerAction (filterBySeverity level msgSeverity)
{-# INLINE setLoggerLevel #-}

outputLoggerToFile :: (Emb IO :> es, ExceptionE :> es) => FilePath -> Eff (Log Text ': es) a -> Eff es a
outputLoggerToFile path f = 
    handleM >>= (\handle ->
        runLogAction (logTextHandle handle) f)
    where 
        handleM = safeLiftIO $ openFile path AppendMode
{-# INLINE outputLoggerToFile #-}

outputLoggerToStdout :: (Emb IO :> es, ExceptionE :> es) => Eff (Log Text ': es) a -> Eff es a
outputLoggerToStdout = runLogAction logTextStdout
{-# INLINE outputLoggerToStdout #-}

defaultLoggerHandler :: (Emb IO :> es, ExceptionE :> es) => FilePath -> Severity -> Eff (Log Message ': es) a -> Eff es a
defaultLoggerHandler fp serve = 
    outputLoggerToFile fp . fmtLogger . setLoggerLevel serve
{-# INLINE defaultLoggerHandler #-}

log1 :: (LogE :> es) => Severity -> Text -> Eff es ()
log1 sever msg = withFrozenCallStack $ log msg1
    where
        msg1 = Msg {
            msgSeverity = sever,
            msgStack = callStack,
            msgText = msg        
        }   
{-# INLINE log1 #-}

logError :: (LogE :> es) => Text -> Eff es ()
logError = log1 Error
{-# INLINE logError #-}
logDebug :: (LogE :> es) => Text -> Eff es ()
logDebug = log1 Debug
{-# INLINE logDebug #-}
logInfo :: (LogE :> es) => Text -> Eff es ()
logInfo = log1 Info
{-# INLINE logInfo #-}
logWarning :: (LogE :> es) => Text -> Eff es ()
logWarning = log1 Warning
{-# INLINE logWarning #-}