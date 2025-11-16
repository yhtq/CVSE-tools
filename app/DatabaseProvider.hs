module DatabaseProvider where

import Control.Monad.Hefty (
    raise,
    FOEs,
    reinterpret,
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


newtype DataBaseHandler = DataBaseHandler Pipe
newtype DataBaseTableHandler = DataBaseTableHandler (Pipe, Database)
type DataBaseState = Ask DataBaseHandler
type DataBaseTableState = Ask DataBaseTableHandler

data DatabaseIO :: Effect where
    RunDbAction :: forall a f. Action IO a -> DatabaseIO f a
makeEffectF ''DatabaseIO

runInDatabase :: forall es. (Emb IO :> es, FOEs es) => Host -> (Eff (DataBaseState ': es) ~> Eff es)
runInDatabase host = interpretWith (
        \Ask f -> do
            pipe <- liftIO $ connect host
            result <- f (DataBaseHandler pipe)
            liftIO $ close pipe
            pure result
    )
runInDatabaseTable :: forall es. Database -> (Eff (DataBaseTableState ': es) ~> Eff (DataBaseState ': es))
runInDatabaseTable db = reinterpret \case
    Ask -> do
        (DataBaseHandler pipe) <- ask
        pure $ DataBaseTableHandler (pipe, db)

runDatabaseIO :: forall es. (Emb IO :> es, DataBaseTableState :> es) => Eff (DatabaseIO ': es) ~> Eff es
runDatabaseIO = interpret \case
    RunDbAction action -> do
        (DataBaseTableHandler (pipe, db)) <- ask
        liftIO $ access pipe master db action