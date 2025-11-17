module DatabaseProvider where

import Control.Exception.Base(try, catch, SomeException, ErrorCall)
import Data.Effect.Except (Throw, throw)
import Control.Monad.Hefty (
    raise,
    AlgHandler,
    Throw(..),
    FOEs,
    reinterpret,
    reinterpretWith,
    Eff,
    Effect,
    Emb,
    Freer,
    RemoveExps,
    UnliftIO,
    State,
    interprets,
    interpret,
    interpretBy,
    interpretWith,
    liftIO,
    makeEffectF,
    makeEffectH,
    nil,
    transform,
    (!:),
    type (:>),
    type (~>),
    (&), rewrite, Ask(..)
 )
import Database.MongoDB (Pipe, Database, Action, master, access, connect, close, Host)
import Control.Monad.Hefty.Reader (runAsk, ask)
import Data.Text (Text, pack)
import Control.Monad.Hefty.Except (Catch, runCatch)
import qualified Control.Monad.Hefty.Except as Except


newtype DataBaseHandler = DataBaseHandler Pipe
newtype DataBaseTableHandler = DataBaseTableHandler (Pipe, Database)
type DataBaseState = Ask DataBaseHandler
type DataBaseTableState = Ask DataBaseTableHandler
type ErrMessage = Text

runThrow :: (FOEs es) => Eff (Throw e ': es) a -> Eff es (Either e a)
runThrow = interpretBy (pure . Right) handleThrow

runThrowPrint :: (Emb IO :> es, FOEs es, Show e) => Eff (Throw e ': es) a -> Eff es (Maybe a)
runThrowPrint f = do
    result <- runThrow f
    case result of
        Right val -> pure (Just val)
        Left err -> do
            liftIO $ putStrLn ("Error: " ++ show err)
            pure Nothing

handleThrow :: Applicative g => AlgHandler (Throw e) f g (Either e a)
handleThrow (Throw e) _ = pure $ Left e

data DatabaseIO :: Effect where
    RunDbAction :: forall a f. Action IO a -> DatabaseIO f a
makeEffectF ''DatabaseIO

-- wrap all IO exceptions into ErrMessage
safeLiftIO :: forall es a. (Emb IO :> es, Throw ErrMessage :> es) => IO a -> Eff es a
safeLiftIO c = do
    io_result <- liftIO $ do
        catch (Right <$> c)
            (\(e :: SomeException) -> return $ Left (pack $ show e))
    case io_result of
        Right val -> pure val
        Left err -> throw err

runInDatabase :: forall es. (Emb IO :> es, Throw ErrMessage :> es, FOEs es) => Host -> (Eff (DataBaseState ': es) ~> Eff (Catch ErrMessage ': es))
runInDatabase host = reinterpretWith __handler 
        where __handler :: AlgHandler DataBaseState f (Eff (Catch ErrMessage ': es)) a 
              __handler Ask f = do
                pipe <- safeLiftIO $ connect host
                safeLiftIO $ putStrLn "Database connected."
                result <- Except.catch 
                    (Right <$> f (DataBaseHandler pipe))
                    (\(e :: ErrMessage) -> pure $ Left e)
                safeLiftIO $ putStrLn "Database connection closed."
                safeLiftIO $ close pipe
                case result of
                    Right val -> pure val
                    Left err -> throw err
        
runInDatabaseTable :: forall es. Database -> (Eff (DataBaseTableState ': es) ~> Eff (DataBaseState ': es))
runInDatabaseTable db = reinterpret \case
    Ask -> do
        (DataBaseHandler pipe) <- ask
        pure $ DataBaseTableHandler (pipe, db)

runDatabaseIO :: forall es. (Emb IO :> es, Throw ErrMessage :> es, DataBaseTableState :> es) => Eff (DatabaseIO ': es) ~> Eff es
runDatabaseIO = interpret \case
    RunDbAction action -> do
        (DataBaseTableHandler (pipe, db)) <- ask
        safeLiftIO $ access pipe master db action