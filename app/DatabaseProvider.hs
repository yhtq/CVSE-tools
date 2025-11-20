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
    State,
    interprets,
    interpret,
    interpretBy,
    interpretWith,
    liftIO,
    makeEffectF,
    makeEffectH,
    transform,
    type (:>),
    type (~>),
    (&), rewrite, Ask(..)
 )
import Database.MongoDB (Pipe, Database, Action, master, access, connect, close, Host)
import Control.Monad.Hefty.Reader (runAsk, ask)
import Data.Text (Text, pack)
import Control.Monad.Hefty.Except (Catch, runCatch)
import qualified Control.Monad.Hefty.Except as Except
import Logger 


newtype DataBaseHandler = DataBaseHandler Pipe
newtype DataBaseTableHandler = DataBaseTableHandler (Pipe, Database)
type DataBaseState = Ask DataBaseHandler
type DataBaseTableState = Ask DataBaseTableHandler

runThrow :: (FOEs es) => Eff (Throw e ': es) a -> Eff es (Either e a)
runThrow = interpretBy (pure . Right) handleThrow

runThrowPrint :: (Emb IO :> es, FOEs es, Show e) => Eff (Throw e ': es) a -> Eff es (Maybe a)
runThrowPrint f = do
    result <- runThrow f
    case result of
        Right val -> pure (Just val)
        Left err -> do
            liftIO $ putStrLn ("Uncatched Error: " ++ show err)
            pure Nothing

handleThrow :: Applicative g => AlgHandler (Throw e) f g (Either e a)
handleThrow (Throw e) _ = pure $ Left e

data DatabaseIO :: Effect where
    RunDbAction :: forall a f. Action IO a -> DatabaseIO f a
makeEffectF ''DatabaseIO

runInDatabase :: forall es. (Emb IO :> es, Throw ErrMessage :> es, LogE :> es, FOEs es) => 
    Host -> (Eff (DataBaseState ': es) ~> Eff (Catch ErrMessage ': es))
runInDatabase host = reinterpretWith __handler 
        where __handler :: AlgHandler DataBaseState f (Eff (Catch ErrMessage ': es)) a 
              __handler Ask f = do
                pipe <- safeLiftIO $ connect host
                result <- Except.catch 
                    (Right <$> f (DataBaseHandler pipe))
                    (\(e :: ErrMessage) -> do
                        $(logDebug) $ "Database connection error: " <> e
                        pure $ Left e)
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