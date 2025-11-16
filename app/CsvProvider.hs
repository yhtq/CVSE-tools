{-# LANGUAGE DeriveGeneric #-}
module CsvProvider where
import Data.Csv(FromNamedRecord, decode, HasHeader(..), decodeByName)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO.Utf8 qualified as TIO
import Data.ByteString qualified as BL
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)

import Control.Monad.Hefty (
    Eff,
    Effect,
    Emb,
    Freer,
    RemoveExps,
    UnliftIO,
    interprets,
    interpret,
    liftIO,
    makeEffectF,
    makeEffectH,
    nil,
    (!:),
    type (:>),
    type (~>),
 )
import Control.Monad.Hefty.Provider (Provider, provide_, runRegionProvider_)
import Control.Monad.Hefty.Unlift (runUnliftIO)

data CsvIO :: Effect where
    ReadCsv :: forall a f. FromNamedRecord a => FilePath -> CsvIO f (Vector a)
makeEffectF ''CsvIO

csvIOToIO :: forall es. (Emb IO :> es) => Eff (CsvIO ': es) ~> Eff es
csvIOToIO = interpret \case
    ReadCsv path -> do
        content <- liftIO $ TIO.readFile path
        let result = decodeByName $ BL.fromStrict $ encodeUtf8 content
        case result of
            Left err -> error $ "CSV parsing error: " <> err
            Right v  -> pure $ snd v
        