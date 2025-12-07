{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.CVSEAPI.CVSE where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers as GH
import qualified Capnp.Classes as C
import qualified GHC.Generics as Generics
import qualified Capnp.GenHelpers.Rpc as GH
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Cvse 
type instance (R.ReprFor Cvse) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Cvse) where
    typeId  = 11881061825597523281
instance (C.Parse Cvse (GH.Client Cvse)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Cvse) where
    type Server Cvse = Cvse'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Cvse)) [(GH.toUntypedMethodHandler ((cvse'updateModifyEntry) s_))
                                                                      ,(GH.toUntypedMethodHandler ((cvse'updateNewEntry) s_))
                                                                      ,(GH.toUntypedMethodHandler ((cvse'updateRecordingDataEntry) s_))
                                                                      ,(GH.toUntypedMethodHandler ((cvse'getAll) s_))
                                                                      ,(GH.toUntypedMethodHandler ((cvse'lookupMetaInfo) s_))
                                                                      ,(GH.toUntypedMethodHandler ((cvse'lookupDataInfo) s_))
                                                                      ,(GH.toUntypedMethodHandler ((cvse'lookupOneDataInfo) s_))
                                                                      ,(GH.toUntypedMethodHandler ((cvse'reCalculateRankings) s_))
                                                                      ,(GH.toUntypedMethodHandler ((cvse'getAllRankingInfo) s_))] [])
class (Cvse'server_ s_) where
    {-# MINIMAL cvse'updateModifyEntry,cvse'updateNewEntry,cvse'updateRecordingDataEntry,cvse'getAll,cvse'lookupMetaInfo,cvse'lookupDataInfo,cvse'lookupOneDataInfo,cvse'reCalculateRankings,cvse'getAllRankingInfo #-}
    cvse'updateModifyEntry :: s_ -> (GH.MethodHandler Cvse'updateModifyEntry'params Cvse'updateModifyEntry'results)
    cvse'updateModifyEntry _ = GH.methodUnimplemented
    cvse'updateNewEntry :: s_ -> (GH.MethodHandler Cvse'updateNewEntry'params Cvse'updateNewEntry'results)
    cvse'updateNewEntry _ = GH.methodUnimplemented
    cvse'updateRecordingDataEntry :: s_ -> (GH.MethodHandler Cvse'updateRecordingDataEntry'params Cvse'updateRecordingDataEntry'results)
    cvse'updateRecordingDataEntry _ = GH.methodUnimplemented
    cvse'getAll :: s_ -> (GH.MethodHandler Cvse'getAll'params Cvse'getAll'results)
    cvse'getAll _ = GH.methodUnimplemented
    cvse'lookupMetaInfo :: s_ -> (GH.MethodHandler Cvse'lookupMetaInfo'params Cvse'lookupMetaInfo'results)
    cvse'lookupMetaInfo _ = GH.methodUnimplemented
    cvse'lookupDataInfo :: s_ -> (GH.MethodHandler Cvse'lookupDataInfo'params Cvse'lookupDataInfo'results)
    cvse'lookupDataInfo _ = GH.methodUnimplemented
    cvse'lookupOneDataInfo :: s_ -> (GH.MethodHandler Cvse'lookupOneDataInfo'params Cvse'lookupOneDataInfo'results)
    cvse'lookupOneDataInfo _ = GH.methodUnimplemented
    cvse'reCalculateRankings :: s_ -> (GH.MethodHandler Cvse'reCalculateRankings'params Cvse'reCalculateRankings'results)
    cvse'reCalculateRankings _ = GH.methodUnimplemented
    cvse'getAllRankingInfo :: s_ -> (GH.MethodHandler Cvse'getAllRankingInfo'params Cvse'getAllRankingInfo'results)
    cvse'getAllRankingInfo _ = GH.methodUnimplemented
instance (GH.HasMethod "updateModifyEntry" Cvse Cvse'updateModifyEntry'params Cvse'updateModifyEntry'results) where
    methodByLabel  = (GH.Method 11881061825597523281 0)
instance (GH.HasMethod "updateNewEntry" Cvse Cvse'updateNewEntry'params Cvse'updateNewEntry'results) where
    methodByLabel  = (GH.Method 11881061825597523281 1)
instance (GH.HasMethod "updateRecordingDataEntry" Cvse Cvse'updateRecordingDataEntry'params Cvse'updateRecordingDataEntry'results) where
    methodByLabel  = (GH.Method 11881061825597523281 2)
instance (GH.HasMethod "getAll" Cvse Cvse'getAll'params Cvse'getAll'results) where
    methodByLabel  = (GH.Method 11881061825597523281 3)
instance (GH.HasMethod "lookupMetaInfo" Cvse Cvse'lookupMetaInfo'params Cvse'lookupMetaInfo'results) where
    methodByLabel  = (GH.Method 11881061825597523281 4)
instance (GH.HasMethod "lookupDataInfo" Cvse Cvse'lookupDataInfo'params Cvse'lookupDataInfo'results) where
    methodByLabel  = (GH.Method 11881061825597523281 5)
instance (GH.HasMethod "lookupOneDataInfo" Cvse Cvse'lookupOneDataInfo'params Cvse'lookupOneDataInfo'results) where
    methodByLabel  = (GH.Method 11881061825597523281 6)
instance (GH.HasMethod "reCalculateRankings" Cvse Cvse'reCalculateRankings'params Cvse'reCalculateRankings'results) where
    methodByLabel  = (GH.Method 11881061825597523281 7)
instance (GH.HasMethod "getAllRankingInfo" Cvse Cvse'getAllRankingInfo'params Cvse'getAllRankingInfo'results) where
    methodByLabel  = (GH.Method 11881061825597523281 8)
data Cvse'updateModifyEntry'params 
type instance (R.ReprFor Cvse'updateModifyEntry'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'updateModifyEntry'params) where
    typeId  = 9856680309677037086
instance (C.TypedStruct Cvse'updateModifyEntry'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Cvse'updateModifyEntry'params) where
    type AllocHint Cvse'updateModifyEntry'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'updateModifyEntry'params (C.Parsed Cvse'updateModifyEntry'params))
instance (C.AllocateList Cvse'updateModifyEntry'params) where
    type ListAllocHint Cvse'updateModifyEntry'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'updateModifyEntry'params (C.Parsed Cvse'updateModifyEntry'params))
data instance C.Parsed Cvse'updateModifyEntry'params
    = Cvse'updateModifyEntry'params 
        {entries :: (RP.Parsed (R.List Cvse'ModifyEntry))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'updateModifyEntry'params))
deriving instance (Std_.Eq (C.Parsed Cvse'updateModifyEntry'params))
instance (C.Parse Cvse'updateModifyEntry'params (C.Parsed Cvse'updateModifyEntry'params)) where
    parse raw_ = (Cvse'updateModifyEntry'params <$> (GH.parseField #entries raw_))
instance (C.Marshal Cvse'updateModifyEntry'params (C.Parsed Cvse'updateModifyEntry'params)) where
    marshalInto raw_ Cvse'updateModifyEntry'params{..} = (do
        (GH.encodeField #entries entries raw_)
        (Std_.pure ())
        )
instance (GH.HasField "entries" GH.Slot Cvse'updateModifyEntry'params (R.List Cvse'ModifyEntry)) where
    fieldByLabel  = (GH.ptrField 0)
data Cvse'updateModifyEntry'results 
type instance (R.ReprFor Cvse'updateModifyEntry'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'updateModifyEntry'results) where
    typeId  = 12390566660365021479
instance (C.TypedStruct Cvse'updateModifyEntry'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Cvse'updateModifyEntry'results) where
    type AllocHint Cvse'updateModifyEntry'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'updateModifyEntry'results (C.Parsed Cvse'updateModifyEntry'results))
instance (C.AllocateList Cvse'updateModifyEntry'results) where
    type ListAllocHint Cvse'updateModifyEntry'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'updateModifyEntry'results (C.Parsed Cvse'updateModifyEntry'results))
data instance C.Parsed Cvse'updateModifyEntry'results
    = Cvse'updateModifyEntry'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'updateModifyEntry'results))
deriving instance (Std_.Eq (C.Parsed Cvse'updateModifyEntry'results))
instance (C.Parse Cvse'updateModifyEntry'results (C.Parsed Cvse'updateModifyEntry'results)) where
    parse raw_ = (Std_.pure Cvse'updateModifyEntry'results)
instance (C.Marshal Cvse'updateModifyEntry'results (C.Parsed Cvse'updateModifyEntry'results)) where
    marshalInto _raw (Cvse'updateModifyEntry'results) = (Std_.pure ())
data Cvse'updateNewEntry'params 
type instance (R.ReprFor Cvse'updateNewEntry'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'updateNewEntry'params) where
    typeId  = 17187972643465061944
instance (C.TypedStruct Cvse'updateNewEntry'params) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Cvse'updateNewEntry'params) where
    type AllocHint Cvse'updateNewEntry'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'updateNewEntry'params (C.Parsed Cvse'updateNewEntry'params))
instance (C.AllocateList Cvse'updateNewEntry'params) where
    type ListAllocHint Cvse'updateNewEntry'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'updateNewEntry'params (C.Parsed Cvse'updateNewEntry'params))
data instance C.Parsed Cvse'updateNewEntry'params
    = Cvse'updateNewEntry'params 
        {entries :: (RP.Parsed (R.List Cvse'RecordingNewEntry))
        ,replace :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'updateNewEntry'params))
deriving instance (Std_.Eq (C.Parsed Cvse'updateNewEntry'params))
instance (C.Parse Cvse'updateNewEntry'params (C.Parsed Cvse'updateNewEntry'params)) where
    parse raw_ = (Cvse'updateNewEntry'params <$> (GH.parseField #entries raw_)
                                             <*> (GH.parseField #replace raw_))
instance (C.Marshal Cvse'updateNewEntry'params (C.Parsed Cvse'updateNewEntry'params)) where
    marshalInto raw_ Cvse'updateNewEntry'params{..} = (do
        (GH.encodeField #entries entries raw_)
        (GH.encodeField #replace replace raw_)
        (Std_.pure ())
        )
instance (GH.HasField "entries" GH.Slot Cvse'updateNewEntry'params (R.List Cvse'RecordingNewEntry)) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "replace" GH.Slot Cvse'updateNewEntry'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
data Cvse'updateNewEntry'results 
type instance (R.ReprFor Cvse'updateNewEntry'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'updateNewEntry'results) where
    typeId  = 12578339126877513858
instance (C.TypedStruct Cvse'updateNewEntry'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Cvse'updateNewEntry'results) where
    type AllocHint Cvse'updateNewEntry'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'updateNewEntry'results (C.Parsed Cvse'updateNewEntry'results))
instance (C.AllocateList Cvse'updateNewEntry'results) where
    type ListAllocHint Cvse'updateNewEntry'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'updateNewEntry'results (C.Parsed Cvse'updateNewEntry'results))
data instance C.Parsed Cvse'updateNewEntry'results
    = Cvse'updateNewEntry'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'updateNewEntry'results))
deriving instance (Std_.Eq (C.Parsed Cvse'updateNewEntry'results))
instance (C.Parse Cvse'updateNewEntry'results (C.Parsed Cvse'updateNewEntry'results)) where
    parse raw_ = (Std_.pure Cvse'updateNewEntry'results)
instance (C.Marshal Cvse'updateNewEntry'results (C.Parsed Cvse'updateNewEntry'results)) where
    marshalInto _raw (Cvse'updateNewEntry'results) = (Std_.pure ())
data Cvse'updateRecordingDataEntry'params 
type instance (R.ReprFor Cvse'updateRecordingDataEntry'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'updateRecordingDataEntry'params) where
    typeId  = 18402389460281104284
instance (C.TypedStruct Cvse'updateRecordingDataEntry'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Cvse'updateRecordingDataEntry'params) where
    type AllocHint Cvse'updateRecordingDataEntry'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'updateRecordingDataEntry'params (C.Parsed Cvse'updateRecordingDataEntry'params))
instance (C.AllocateList Cvse'updateRecordingDataEntry'params) where
    type ListAllocHint Cvse'updateRecordingDataEntry'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'updateRecordingDataEntry'params (C.Parsed Cvse'updateRecordingDataEntry'params))
data instance C.Parsed Cvse'updateRecordingDataEntry'params
    = Cvse'updateRecordingDataEntry'params 
        {entries :: (RP.Parsed (R.List Cvse'RecordingDataEntry))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'updateRecordingDataEntry'params))
deriving instance (Std_.Eq (C.Parsed Cvse'updateRecordingDataEntry'params))
instance (C.Parse Cvse'updateRecordingDataEntry'params (C.Parsed Cvse'updateRecordingDataEntry'params)) where
    parse raw_ = (Cvse'updateRecordingDataEntry'params <$> (GH.parseField #entries raw_))
instance (C.Marshal Cvse'updateRecordingDataEntry'params (C.Parsed Cvse'updateRecordingDataEntry'params)) where
    marshalInto raw_ Cvse'updateRecordingDataEntry'params{..} = (do
        (GH.encodeField #entries entries raw_)
        (Std_.pure ())
        )
instance (GH.HasField "entries" GH.Slot Cvse'updateRecordingDataEntry'params (R.List Cvse'RecordingDataEntry)) where
    fieldByLabel  = (GH.ptrField 0)
data Cvse'updateRecordingDataEntry'results 
type instance (R.ReprFor Cvse'updateRecordingDataEntry'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'updateRecordingDataEntry'results) where
    typeId  = 16742526759880192168
instance (C.TypedStruct Cvse'updateRecordingDataEntry'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Cvse'updateRecordingDataEntry'results) where
    type AllocHint Cvse'updateRecordingDataEntry'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'updateRecordingDataEntry'results (C.Parsed Cvse'updateRecordingDataEntry'results))
instance (C.AllocateList Cvse'updateRecordingDataEntry'results) where
    type ListAllocHint Cvse'updateRecordingDataEntry'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'updateRecordingDataEntry'results (C.Parsed Cvse'updateRecordingDataEntry'results))
data instance C.Parsed Cvse'updateRecordingDataEntry'results
    = Cvse'updateRecordingDataEntry'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'updateRecordingDataEntry'results))
deriving instance (Std_.Eq (C.Parsed Cvse'updateRecordingDataEntry'results))
instance (C.Parse Cvse'updateRecordingDataEntry'results (C.Parsed Cvse'updateRecordingDataEntry'results)) where
    parse raw_ = (Std_.pure Cvse'updateRecordingDataEntry'results)
instance (C.Marshal Cvse'updateRecordingDataEntry'results (C.Parsed Cvse'updateRecordingDataEntry'results)) where
    marshalInto _raw (Cvse'updateRecordingDataEntry'results) = (Std_.pure ())
data Cvse'getAll'params 
type instance (R.ReprFor Cvse'getAll'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'getAll'params) where
    typeId  = 11278317511661808760
instance (C.TypedStruct Cvse'getAll'params) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Cvse'getAll'params) where
    type AllocHint Cvse'getAll'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'getAll'params (C.Parsed Cvse'getAll'params))
instance (C.AllocateList Cvse'getAll'params) where
    type ListAllocHint Cvse'getAll'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'getAll'params (C.Parsed Cvse'getAll'params))
data instance C.Parsed Cvse'getAll'params
    = Cvse'getAll'params 
        {get_unexamined :: (RP.Parsed Std_.Bool)
        ,get_unincluded :: (RP.Parsed Std_.Bool)
        ,from_date :: (RP.Parsed Cvse'Time)
        ,to_date :: (RP.Parsed Cvse'Time)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'getAll'params))
deriving instance (Std_.Eq (C.Parsed Cvse'getAll'params))
instance (C.Parse Cvse'getAll'params (C.Parsed Cvse'getAll'params)) where
    parse raw_ = (Cvse'getAll'params <$> (GH.parseField #get_unexamined raw_)
                                     <*> (GH.parseField #get_unincluded raw_)
                                     <*> (GH.parseField #from_date raw_)
                                     <*> (GH.parseField #to_date raw_))
instance (C.Marshal Cvse'getAll'params (C.Parsed Cvse'getAll'params)) where
    marshalInto raw_ Cvse'getAll'params{..} = (do
        (GH.encodeField #get_unexamined get_unexamined raw_)
        (GH.encodeField #get_unincluded get_unincluded raw_)
        (GH.encodeField #from_date from_date raw_)
        (GH.encodeField #to_date to_date raw_)
        (Std_.pure ())
        )
instance (GH.HasField "get_unexamined" GH.Slot Cvse'getAll'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "get_unincluded" GH.Slot Cvse'getAll'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 0 1 0)
instance (GH.HasField "from_date" GH.Slot Cvse'getAll'params Cvse'Time) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "to_date" GH.Slot Cvse'getAll'params Cvse'Time) where
    fieldByLabel  = (GH.ptrField 1)
data Cvse'getAll'results 
type instance (R.ReprFor Cvse'getAll'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'getAll'results) where
    typeId  = 16122335308505648362
instance (C.TypedStruct Cvse'getAll'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Cvse'getAll'results) where
    type AllocHint Cvse'getAll'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'getAll'results (C.Parsed Cvse'getAll'results))
instance (C.AllocateList Cvse'getAll'results) where
    type ListAllocHint Cvse'getAll'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'getAll'results (C.Parsed Cvse'getAll'results))
data instance C.Parsed Cvse'getAll'results
    = Cvse'getAll'results 
        {indices :: (RP.Parsed (R.List Cvse'Index))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'getAll'results))
deriving instance (Std_.Eq (C.Parsed Cvse'getAll'results))
instance (C.Parse Cvse'getAll'results (C.Parsed Cvse'getAll'results)) where
    parse raw_ = (Cvse'getAll'results <$> (GH.parseField #indices raw_))
instance (C.Marshal Cvse'getAll'results (C.Parsed Cvse'getAll'results)) where
    marshalInto raw_ Cvse'getAll'results{..} = (do
        (GH.encodeField #indices indices raw_)
        (Std_.pure ())
        )
instance (GH.HasField "indices" GH.Slot Cvse'getAll'results (R.List Cvse'Index)) where
    fieldByLabel  = (GH.ptrField 0)
data Cvse'lookupMetaInfo'params 
type instance (R.ReprFor Cvse'lookupMetaInfo'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'lookupMetaInfo'params) where
    typeId  = 10786361039257189953
instance (C.TypedStruct Cvse'lookupMetaInfo'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Cvse'lookupMetaInfo'params) where
    type AllocHint Cvse'lookupMetaInfo'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'lookupMetaInfo'params (C.Parsed Cvse'lookupMetaInfo'params))
instance (C.AllocateList Cvse'lookupMetaInfo'params) where
    type ListAllocHint Cvse'lookupMetaInfo'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'lookupMetaInfo'params (C.Parsed Cvse'lookupMetaInfo'params))
data instance C.Parsed Cvse'lookupMetaInfo'params
    = Cvse'lookupMetaInfo'params 
        {indices :: (RP.Parsed (R.List Cvse'Index))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'lookupMetaInfo'params))
deriving instance (Std_.Eq (C.Parsed Cvse'lookupMetaInfo'params))
instance (C.Parse Cvse'lookupMetaInfo'params (C.Parsed Cvse'lookupMetaInfo'params)) where
    parse raw_ = (Cvse'lookupMetaInfo'params <$> (GH.parseField #indices raw_))
instance (C.Marshal Cvse'lookupMetaInfo'params (C.Parsed Cvse'lookupMetaInfo'params)) where
    marshalInto raw_ Cvse'lookupMetaInfo'params{..} = (do
        (GH.encodeField #indices indices raw_)
        (Std_.pure ())
        )
instance (GH.HasField "indices" GH.Slot Cvse'lookupMetaInfo'params (R.List Cvse'Index)) where
    fieldByLabel  = (GH.ptrField 0)
data Cvse'lookupMetaInfo'results 
type instance (R.ReprFor Cvse'lookupMetaInfo'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'lookupMetaInfo'results) where
    typeId  = 12122321499878525690
instance (C.TypedStruct Cvse'lookupMetaInfo'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Cvse'lookupMetaInfo'results) where
    type AllocHint Cvse'lookupMetaInfo'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'lookupMetaInfo'results (C.Parsed Cvse'lookupMetaInfo'results))
instance (C.AllocateList Cvse'lookupMetaInfo'results) where
    type ListAllocHint Cvse'lookupMetaInfo'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'lookupMetaInfo'results (C.Parsed Cvse'lookupMetaInfo'results))
data instance C.Parsed Cvse'lookupMetaInfo'results
    = Cvse'lookupMetaInfo'results 
        {entries :: (RP.Parsed (R.List Cvse'RecordingNewEntry))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'lookupMetaInfo'results))
deriving instance (Std_.Eq (C.Parsed Cvse'lookupMetaInfo'results))
instance (C.Parse Cvse'lookupMetaInfo'results (C.Parsed Cvse'lookupMetaInfo'results)) where
    parse raw_ = (Cvse'lookupMetaInfo'results <$> (GH.parseField #entries raw_))
instance (C.Marshal Cvse'lookupMetaInfo'results (C.Parsed Cvse'lookupMetaInfo'results)) where
    marshalInto raw_ Cvse'lookupMetaInfo'results{..} = (do
        (GH.encodeField #entries entries raw_)
        (Std_.pure ())
        )
instance (GH.HasField "entries" GH.Slot Cvse'lookupMetaInfo'results (R.List Cvse'RecordingNewEntry)) where
    fieldByLabel  = (GH.ptrField 0)
data Cvse'lookupDataInfo'params 
type instance (R.ReprFor Cvse'lookupDataInfo'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'lookupDataInfo'params) where
    typeId  = 14056913260755409807
instance (C.TypedStruct Cvse'lookupDataInfo'params) where
    numStructWords  = 0
    numStructPtrs  = 3
instance (C.Allocate Cvse'lookupDataInfo'params) where
    type AllocHint Cvse'lookupDataInfo'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'lookupDataInfo'params (C.Parsed Cvse'lookupDataInfo'params))
instance (C.AllocateList Cvse'lookupDataInfo'params) where
    type ListAllocHint Cvse'lookupDataInfo'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'lookupDataInfo'params (C.Parsed Cvse'lookupDataInfo'params))
data instance C.Parsed Cvse'lookupDataInfo'params
    = Cvse'lookupDataInfo'params 
        {indices :: (RP.Parsed (R.List Cvse'Index))
        ,from_date :: (RP.Parsed Cvse'Time)
        ,to_date :: (RP.Parsed Cvse'Time)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'lookupDataInfo'params))
deriving instance (Std_.Eq (C.Parsed Cvse'lookupDataInfo'params))
instance (C.Parse Cvse'lookupDataInfo'params (C.Parsed Cvse'lookupDataInfo'params)) where
    parse raw_ = (Cvse'lookupDataInfo'params <$> (GH.parseField #indices raw_)
                                             <*> (GH.parseField #from_date raw_)
                                             <*> (GH.parseField #to_date raw_))
instance (C.Marshal Cvse'lookupDataInfo'params (C.Parsed Cvse'lookupDataInfo'params)) where
    marshalInto raw_ Cvse'lookupDataInfo'params{..} = (do
        (GH.encodeField #indices indices raw_)
        (GH.encodeField #from_date from_date raw_)
        (GH.encodeField #to_date to_date raw_)
        (Std_.pure ())
        )
instance (GH.HasField "indices" GH.Slot Cvse'lookupDataInfo'params (R.List Cvse'Index)) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "from_date" GH.Slot Cvse'lookupDataInfo'params Cvse'Time) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "to_date" GH.Slot Cvse'lookupDataInfo'params Cvse'Time) where
    fieldByLabel  = (GH.ptrField 2)
data Cvse'lookupDataInfo'results 
type instance (R.ReprFor Cvse'lookupDataInfo'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'lookupDataInfo'results) where
    typeId  = 10461453117351809676
instance (C.TypedStruct Cvse'lookupDataInfo'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Cvse'lookupDataInfo'results) where
    type AllocHint Cvse'lookupDataInfo'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'lookupDataInfo'results (C.Parsed Cvse'lookupDataInfo'results))
instance (C.AllocateList Cvse'lookupDataInfo'results) where
    type ListAllocHint Cvse'lookupDataInfo'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'lookupDataInfo'results (C.Parsed Cvse'lookupDataInfo'results))
data instance C.Parsed Cvse'lookupDataInfo'results
    = Cvse'lookupDataInfo'results 
        {entries :: (RP.Parsed (R.List (R.List Cvse'RecordingDataEntry)))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'lookupDataInfo'results))
deriving instance (Std_.Eq (C.Parsed Cvse'lookupDataInfo'results))
instance (C.Parse Cvse'lookupDataInfo'results (C.Parsed Cvse'lookupDataInfo'results)) where
    parse raw_ = (Cvse'lookupDataInfo'results <$> (GH.parseField #entries raw_))
instance (C.Marshal Cvse'lookupDataInfo'results (C.Parsed Cvse'lookupDataInfo'results)) where
    marshalInto raw_ Cvse'lookupDataInfo'results{..} = (do
        (GH.encodeField #entries entries raw_)
        (Std_.pure ())
        )
instance (GH.HasField "entries" GH.Slot Cvse'lookupDataInfo'results (R.List (R.List Cvse'RecordingDataEntry))) where
    fieldByLabel  = (GH.ptrField 0)
data Cvse'lookupOneDataInfo'params 
type instance (R.ReprFor Cvse'lookupOneDataInfo'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'lookupOneDataInfo'params) where
    typeId  = 14567435411466230623
instance (C.TypedStruct Cvse'lookupOneDataInfo'params) where
    numStructWords  = 0
    numStructPtrs  = 3
instance (C.Allocate Cvse'lookupOneDataInfo'params) where
    type AllocHint Cvse'lookupOneDataInfo'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'lookupOneDataInfo'params (C.Parsed Cvse'lookupOneDataInfo'params))
instance (C.AllocateList Cvse'lookupOneDataInfo'params) where
    type ListAllocHint Cvse'lookupOneDataInfo'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'lookupOneDataInfo'params (C.Parsed Cvse'lookupOneDataInfo'params))
data instance C.Parsed Cvse'lookupOneDataInfo'params
    = Cvse'lookupOneDataInfo'params 
        {indices :: (RP.Parsed (R.List Cvse'Index))
        ,from_date :: (RP.Parsed Cvse'Time)
        ,to_date :: (RP.Parsed Cvse'Time)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'lookupOneDataInfo'params))
deriving instance (Std_.Eq (C.Parsed Cvse'lookupOneDataInfo'params))
instance (C.Parse Cvse'lookupOneDataInfo'params (C.Parsed Cvse'lookupOneDataInfo'params)) where
    parse raw_ = (Cvse'lookupOneDataInfo'params <$> (GH.parseField #indices raw_)
                                                <*> (GH.parseField #from_date raw_)
                                                <*> (GH.parseField #to_date raw_))
instance (C.Marshal Cvse'lookupOneDataInfo'params (C.Parsed Cvse'lookupOneDataInfo'params)) where
    marshalInto raw_ Cvse'lookupOneDataInfo'params{..} = (do
        (GH.encodeField #indices indices raw_)
        (GH.encodeField #from_date from_date raw_)
        (GH.encodeField #to_date to_date raw_)
        (Std_.pure ())
        )
instance (GH.HasField "indices" GH.Slot Cvse'lookupOneDataInfo'params (R.List Cvse'Index)) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "from_date" GH.Slot Cvse'lookupOneDataInfo'params Cvse'Time) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "to_date" GH.Slot Cvse'lookupOneDataInfo'params Cvse'Time) where
    fieldByLabel  = (GH.ptrField 2)
data Cvse'lookupOneDataInfo'results 
type instance (R.ReprFor Cvse'lookupOneDataInfo'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'lookupOneDataInfo'results) where
    typeId  = 15015279242320443143
instance (C.TypedStruct Cvse'lookupOneDataInfo'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Cvse'lookupOneDataInfo'results) where
    type AllocHint Cvse'lookupOneDataInfo'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'lookupOneDataInfo'results (C.Parsed Cvse'lookupOneDataInfo'results))
instance (C.AllocateList Cvse'lookupOneDataInfo'results) where
    type ListAllocHint Cvse'lookupOneDataInfo'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'lookupOneDataInfo'results (C.Parsed Cvse'lookupOneDataInfo'results))
data instance C.Parsed Cvse'lookupOneDataInfo'results
    = Cvse'lookupOneDataInfo'results 
        {entries :: (RP.Parsed (R.List Cvse'RecordingDataEntry))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'lookupOneDataInfo'results))
deriving instance (Std_.Eq (C.Parsed Cvse'lookupOneDataInfo'results))
instance (C.Parse Cvse'lookupOneDataInfo'results (C.Parsed Cvse'lookupOneDataInfo'results)) where
    parse raw_ = (Cvse'lookupOneDataInfo'results <$> (GH.parseField #entries raw_))
instance (C.Marshal Cvse'lookupOneDataInfo'results (C.Parsed Cvse'lookupOneDataInfo'results)) where
    marshalInto raw_ Cvse'lookupOneDataInfo'results{..} = (do
        (GH.encodeField #entries entries raw_)
        (Std_.pure ())
        )
instance (GH.HasField "entries" GH.Slot Cvse'lookupOneDataInfo'results (R.List Cvse'RecordingDataEntry)) where
    fieldByLabel  = (GH.ptrField 0)
data Cvse'reCalculateRankings'params 
type instance (R.ReprFor Cvse'reCalculateRankings'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'reCalculateRankings'params) where
    typeId  = 15435079461487451845
instance (C.TypedStruct Cvse'reCalculateRankings'params) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Cvse'reCalculateRankings'params) where
    type AllocHint Cvse'reCalculateRankings'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'reCalculateRankings'params (C.Parsed Cvse'reCalculateRankings'params))
instance (C.AllocateList Cvse'reCalculateRankings'params) where
    type ListAllocHint Cvse'reCalculateRankings'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'reCalculateRankings'params (C.Parsed Cvse'reCalculateRankings'params))
data instance C.Parsed Cvse'reCalculateRankings'params
    = Cvse'reCalculateRankings'params 
        {rank :: (RP.Parsed Cvse'Rank)
        ,index :: (RP.Parsed Std_.Int32)
        ,contain_unexamined :: (RP.Parsed Std_.Bool)
        ,lock :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'reCalculateRankings'params))
deriving instance (Std_.Eq (C.Parsed Cvse'reCalculateRankings'params))
instance (C.Parse Cvse'reCalculateRankings'params (C.Parsed Cvse'reCalculateRankings'params)) where
    parse raw_ = (Cvse'reCalculateRankings'params <$> (GH.parseField #rank raw_)
                                                  <*> (GH.parseField #index raw_)
                                                  <*> (GH.parseField #contain_unexamined raw_)
                                                  <*> (GH.parseField #lock raw_))
instance (C.Marshal Cvse'reCalculateRankings'params (C.Parsed Cvse'reCalculateRankings'params)) where
    marshalInto raw_ Cvse'reCalculateRankings'params{..} = (do
        (GH.encodeField #rank rank raw_)
        (GH.encodeField #index index raw_)
        (GH.encodeField #contain_unexamined contain_unexamined raw_)
        (GH.encodeField #lock lock raw_)
        (Std_.pure ())
        )
instance (GH.HasField "rank" GH.Slot Cvse'reCalculateRankings'params Cvse'Rank) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "index" GH.Slot Cvse'reCalculateRankings'params Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "contain_unexamined" GH.Slot Cvse'reCalculateRankings'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 0 1 0)
instance (GH.HasField "lock" GH.Slot Cvse'reCalculateRankings'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 33 0 1 0)
data Cvse'reCalculateRankings'results 
type instance (R.ReprFor Cvse'reCalculateRankings'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'reCalculateRankings'results) where
    typeId  = 14741744815618792220
instance (C.TypedStruct Cvse'reCalculateRankings'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Cvse'reCalculateRankings'results) where
    type AllocHint Cvse'reCalculateRankings'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'reCalculateRankings'results (C.Parsed Cvse'reCalculateRankings'results))
instance (C.AllocateList Cvse'reCalculateRankings'results) where
    type ListAllocHint Cvse'reCalculateRankings'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'reCalculateRankings'results (C.Parsed Cvse'reCalculateRankings'results))
data instance C.Parsed Cvse'reCalculateRankings'results
    = Cvse'reCalculateRankings'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'reCalculateRankings'results))
deriving instance (Std_.Eq (C.Parsed Cvse'reCalculateRankings'results))
instance (C.Parse Cvse'reCalculateRankings'results (C.Parsed Cvse'reCalculateRankings'results)) where
    parse raw_ = (Std_.pure Cvse'reCalculateRankings'results)
instance (C.Marshal Cvse'reCalculateRankings'results (C.Parsed Cvse'reCalculateRankings'results)) where
    marshalInto _raw (Cvse'reCalculateRankings'results) = (Std_.pure ())
data Cvse'getAllRankingInfo'params 
type instance (R.ReprFor Cvse'getAllRankingInfo'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'getAllRankingInfo'params) where
    typeId  = 10865088774995854283
instance (C.TypedStruct Cvse'getAllRankingInfo'params) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Cvse'getAllRankingInfo'params) where
    type AllocHint Cvse'getAllRankingInfo'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'getAllRankingInfo'params (C.Parsed Cvse'getAllRankingInfo'params))
instance (C.AllocateList Cvse'getAllRankingInfo'params) where
    type ListAllocHint Cvse'getAllRankingInfo'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'getAllRankingInfo'params (C.Parsed Cvse'getAllRankingInfo'params))
data instance C.Parsed Cvse'getAllRankingInfo'params
    = Cvse'getAllRankingInfo'params 
        {rank :: (RP.Parsed Cvse'Rank)
        ,index :: (RP.Parsed Std_.Int32)
        ,contain_unexamined :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'getAllRankingInfo'params))
deriving instance (Std_.Eq (C.Parsed Cvse'getAllRankingInfo'params))
instance (C.Parse Cvse'getAllRankingInfo'params (C.Parsed Cvse'getAllRankingInfo'params)) where
    parse raw_ = (Cvse'getAllRankingInfo'params <$> (GH.parseField #rank raw_)
                                                <*> (GH.parseField #index raw_)
                                                <*> (GH.parseField #contain_unexamined raw_))
instance (C.Marshal Cvse'getAllRankingInfo'params (C.Parsed Cvse'getAllRankingInfo'params)) where
    marshalInto raw_ Cvse'getAllRankingInfo'params{..} = (do
        (GH.encodeField #rank rank raw_)
        (GH.encodeField #index index raw_)
        (GH.encodeField #contain_unexamined contain_unexamined raw_)
        (Std_.pure ())
        )
instance (GH.HasField "rank" GH.Slot Cvse'getAllRankingInfo'params Cvse'Rank) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "index" GH.Slot Cvse'getAllRankingInfo'params Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "contain_unexamined" GH.Slot Cvse'getAllRankingInfo'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 0 1 0)
data Cvse'getAllRankingInfo'results 
type instance (R.ReprFor Cvse'getAllRankingInfo'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'getAllRankingInfo'results) where
    typeId  = 13438003129499623671
instance (C.TypedStruct Cvse'getAllRankingInfo'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Cvse'getAllRankingInfo'results) where
    type AllocHint Cvse'getAllRankingInfo'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'getAllRankingInfo'results (C.Parsed Cvse'getAllRankingInfo'results))
instance (C.AllocateList Cvse'getAllRankingInfo'results) where
    type ListAllocHint Cvse'getAllRankingInfo'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'getAllRankingInfo'results (C.Parsed Cvse'getAllRankingInfo'results))
data instance C.Parsed Cvse'getAllRankingInfo'results
    = Cvse'getAllRankingInfo'results 
        {entries :: (RP.Parsed (R.List Cvse'RankingInfoEntry))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'getAllRankingInfo'results))
deriving instance (Std_.Eq (C.Parsed Cvse'getAllRankingInfo'results))
instance (C.Parse Cvse'getAllRankingInfo'results (C.Parsed Cvse'getAllRankingInfo'results)) where
    parse raw_ = (Cvse'getAllRankingInfo'results <$> (GH.parseField #entries raw_))
instance (C.Marshal Cvse'getAllRankingInfo'results (C.Parsed Cvse'getAllRankingInfo'results)) where
    marshalInto raw_ Cvse'getAllRankingInfo'results{..} = (do
        (GH.encodeField #entries entries raw_)
        (Std_.pure ())
        )
instance (GH.HasField "entries" GH.Slot Cvse'getAllRankingInfo'results (R.List Cvse'RankingInfoEntry)) where
    fieldByLabel  = (GH.ptrField 0)
data Cvse'Time 
type instance (R.ReprFor Cvse'Time) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'Time) where
    typeId  = 11966071299577555903
instance (C.TypedStruct Cvse'Time) where
    numStructWords  = 2
    numStructPtrs  = 0
instance (C.Allocate Cvse'Time) where
    type AllocHint Cvse'Time = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'Time (C.Parsed Cvse'Time))
instance (C.AllocateList Cvse'Time) where
    type ListAllocHint Cvse'Time = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'Time (C.Parsed Cvse'Time))
data instance C.Parsed Cvse'Time
    = Cvse'Time 
        {seconds :: (RP.Parsed Std_.Int64)
        ,nanoseconds :: (RP.Parsed Std_.Int32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'Time))
deriving instance (Std_.Eq (C.Parsed Cvse'Time))
instance (C.Parse Cvse'Time (C.Parsed Cvse'Time)) where
    parse raw_ = (Cvse'Time <$> (GH.parseField #seconds raw_)
                            <*> (GH.parseField #nanoseconds raw_))
instance (C.Marshal Cvse'Time (C.Parsed Cvse'Time)) where
    marshalInto raw_ Cvse'Time{..} = (do
        (GH.encodeField #seconds seconds raw_)
        (GH.encodeField #nanoseconds nanoseconds raw_)
        (Std_.pure ())
        )
instance (GH.HasField "seconds" GH.Slot Cvse'Time Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "nanoseconds" GH.Slot Cvse'Time Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
data Cvse'Rank 
type instance (R.ReprFor Cvse'Rank) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'Rank) where
    typeId  = 12811600065256265099
instance (C.TypedStruct Cvse'Rank) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Cvse'Rank) where
    type AllocHint Cvse'Rank = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'Rank (C.Parsed Cvse'Rank))
instance (C.AllocateList Cvse'Rank) where
    type ListAllocHint Cvse'Rank = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'Rank (C.Parsed Cvse'Rank))
data instance C.Parsed Cvse'Rank
    = Cvse'Rank 
        {value :: (RP.Parsed Cvse'Rank'RankValue)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'Rank))
deriving instance (Std_.Eq (C.Parsed Cvse'Rank))
instance (C.Parse Cvse'Rank (C.Parsed Cvse'Rank)) where
    parse raw_ = (Cvse'Rank <$> (GH.parseField #value raw_))
instance (C.Marshal Cvse'Rank (C.Parsed Cvse'Rank)) where
    marshalInto raw_ Cvse'Rank{..} = (do
        (GH.encodeField #value value raw_)
        (Std_.pure ())
        )
instance (GH.HasField "value" GH.Slot Cvse'Rank Cvse'Rank'RankValue) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
data Cvse'Rank'RankValue 
    = Cvse'Rank'RankValue'domestic 
    | Cvse'Rank'RankValue'sv 
    | Cvse'Rank'RankValue'utau 
    | Cvse'Rank'RankValue'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor Cvse'Rank'RankValue) = (R.Data R.Sz16)
instance (C.HasTypeId Cvse'Rank'RankValue) where
    typeId  = 13467859512172244533
instance (Std_.Enum Cvse'Rank'RankValue) where
    toEnum n_ = case n_ of
        0 ->
            Cvse'Rank'RankValue'domestic
        1 ->
            Cvse'Rank'RankValue'sv
        2 ->
            Cvse'Rank'RankValue'utau
        tag_ ->
            (Cvse'Rank'RankValue'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (Cvse'Rank'RankValue'domestic) ->
            0
        (Cvse'Rank'RankValue'sv) ->
            1
        (Cvse'Rank'RankValue'utau) ->
            2
        (Cvse'Rank'RankValue'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord Cvse'Rank'RankValue) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse Cvse'Rank'RankValue Cvse'Rank'RankValue) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList Cvse'Rank'RankValue) where
    type ListAllocHint Cvse'Rank'RankValue = Std_.Int
instance (C.EstimateListAlloc Cvse'Rank'RankValue Cvse'Rank'RankValue)
data Cvse'ModifyEntry 
type instance (R.ReprFor Cvse'ModifyEntry) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'ModifyEntry) where
    typeId  = 10532861959111072764
instance (C.TypedStruct Cvse'ModifyEntry) where
    numStructWords  = 1
    numStructPtrs  = 4
instance (C.Allocate Cvse'ModifyEntry) where
    type AllocHint Cvse'ModifyEntry = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'ModifyEntry (C.Parsed Cvse'ModifyEntry))
instance (C.AllocateList Cvse'ModifyEntry) where
    type ListAllocHint Cvse'ModifyEntry = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'ModifyEntry (C.Parsed Cvse'ModifyEntry))
data instance C.Parsed Cvse'ModifyEntry
    = Cvse'ModifyEntry 
        {avid :: (RP.Parsed Basics.Text)
        ,bvid :: (RP.Parsed Basics.Text)
        ,hasIsExamined :: (RP.Parsed Std_.Bool)
        ,isExamined :: (RP.Parsed Std_.Bool)
        ,hasRanks :: (RP.Parsed Std_.Bool)
        ,ranks :: (RP.Parsed (R.List Cvse'Rank))
        ,hasIsRepublish :: (RP.Parsed Std_.Bool)
        ,isRepublish :: (RP.Parsed Std_.Bool)
        ,hasStaffInfo :: (RP.Parsed Std_.Bool)
        ,staffInfo :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'ModifyEntry))
deriving instance (Std_.Eq (C.Parsed Cvse'ModifyEntry))
instance (C.Parse Cvse'ModifyEntry (C.Parsed Cvse'ModifyEntry)) where
    parse raw_ = (Cvse'ModifyEntry <$> (GH.parseField #avid raw_)
                                   <*> (GH.parseField #bvid raw_)
                                   <*> (GH.parseField #hasIsExamined raw_)
                                   <*> (GH.parseField #isExamined raw_)
                                   <*> (GH.parseField #hasRanks raw_)
                                   <*> (GH.parseField #ranks raw_)
                                   <*> (GH.parseField #hasIsRepublish raw_)
                                   <*> (GH.parseField #isRepublish raw_)
                                   <*> (GH.parseField #hasStaffInfo raw_)
                                   <*> (GH.parseField #staffInfo raw_))
instance (C.Marshal Cvse'ModifyEntry (C.Parsed Cvse'ModifyEntry)) where
    marshalInto raw_ Cvse'ModifyEntry{..} = (do
        (GH.encodeField #avid avid raw_)
        (GH.encodeField #bvid bvid raw_)
        (GH.encodeField #hasIsExamined hasIsExamined raw_)
        (GH.encodeField #isExamined isExamined raw_)
        (GH.encodeField #hasRanks hasRanks raw_)
        (GH.encodeField #ranks ranks raw_)
        (GH.encodeField #hasIsRepublish hasIsRepublish raw_)
        (GH.encodeField #isRepublish isRepublish raw_)
        (GH.encodeField #hasStaffInfo hasStaffInfo raw_)
        (GH.encodeField #staffInfo staffInfo raw_)
        (Std_.pure ())
        )
instance (GH.HasField "avid" GH.Slot Cvse'ModifyEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "bvid" GH.Slot Cvse'ModifyEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "hasIsExamined" GH.Slot Cvse'ModifyEntry Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "isExamined" GH.Slot Cvse'ModifyEntry Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 0 1 0)
instance (GH.HasField "hasRanks" GH.Slot Cvse'ModifyEntry Std_.Bool) where
    fieldByLabel  = (GH.dataField 2 0 1 0)
instance (GH.HasField "ranks" GH.Slot Cvse'ModifyEntry (R.List Cvse'Rank)) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "hasIsRepublish" GH.Slot Cvse'ModifyEntry Std_.Bool) where
    fieldByLabel  = (GH.dataField 3 0 1 0)
instance (GH.HasField "isRepublish" GH.Slot Cvse'ModifyEntry Std_.Bool) where
    fieldByLabel  = (GH.dataField 4 0 1 0)
instance (GH.HasField "hasStaffInfo" GH.Slot Cvse'ModifyEntry Std_.Bool) where
    fieldByLabel  = (GH.dataField 5 0 1 0)
instance (GH.HasField "staffInfo" GH.Slot Cvse'ModifyEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
data Cvse'RecordingNewEntry 
type instance (R.ReprFor Cvse'RecordingNewEntry) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'RecordingNewEntry) where
    typeId  = 13146097098386807052
instance (C.TypedStruct Cvse'RecordingNewEntry) where
    numStructWords  = 2
    numStructPtrs  = 11
instance (C.Allocate Cvse'RecordingNewEntry) where
    type AllocHint Cvse'RecordingNewEntry = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'RecordingNewEntry (C.Parsed Cvse'RecordingNewEntry))
instance (C.AllocateList Cvse'RecordingNewEntry) where
    type ListAllocHint Cvse'RecordingNewEntry = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'RecordingNewEntry (C.Parsed Cvse'RecordingNewEntry))
data instance C.Parsed Cvse'RecordingNewEntry
    = Cvse'RecordingNewEntry 
        {avid :: (RP.Parsed Basics.Text)
        ,bvid :: (RP.Parsed Basics.Text)
        ,title :: (RP.Parsed Basics.Text)
        ,uploader :: (RP.Parsed Basics.Text)
        ,upFace :: (RP.Parsed Basics.Text)
        ,copyright :: (RP.Parsed Std_.Int32)
        ,pubdate :: (RP.Parsed Cvse'Time)
        ,duration :: (RP.Parsed Std_.Int32)
        ,page :: (RP.Parsed Std_.Int32)
        ,cover :: (RP.Parsed Basics.Text)
        ,desc :: (RP.Parsed Basics.Text)
        ,tags :: (RP.Parsed (R.List Basics.Text))
        ,isExamined :: (RP.Parsed Std_.Bool)
        ,ranks :: (RP.Parsed (R.List Cvse'Rank))
        ,isRepublish :: (RP.Parsed Std_.Bool)
        ,staffInfo :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'RecordingNewEntry))
deriving instance (Std_.Eq (C.Parsed Cvse'RecordingNewEntry))
instance (C.Parse Cvse'RecordingNewEntry (C.Parsed Cvse'RecordingNewEntry)) where
    parse raw_ = (Cvse'RecordingNewEntry <$> (GH.parseField #avid raw_)
                                         <*> (GH.parseField #bvid raw_)
                                         <*> (GH.parseField #title raw_)
                                         <*> (GH.parseField #uploader raw_)
                                         <*> (GH.parseField #upFace raw_)
                                         <*> (GH.parseField #copyright raw_)
                                         <*> (GH.parseField #pubdate raw_)
                                         <*> (GH.parseField #duration raw_)
                                         <*> (GH.parseField #page raw_)
                                         <*> (GH.parseField #cover raw_)
                                         <*> (GH.parseField #desc raw_)
                                         <*> (GH.parseField #tags raw_)
                                         <*> (GH.parseField #isExamined raw_)
                                         <*> (GH.parseField #ranks raw_)
                                         <*> (GH.parseField #isRepublish raw_)
                                         <*> (GH.parseField #staffInfo raw_))
instance (C.Marshal Cvse'RecordingNewEntry (C.Parsed Cvse'RecordingNewEntry)) where
    marshalInto raw_ Cvse'RecordingNewEntry{..} = (do
        (GH.encodeField #avid avid raw_)
        (GH.encodeField #bvid bvid raw_)
        (GH.encodeField #title title raw_)
        (GH.encodeField #uploader uploader raw_)
        (GH.encodeField #upFace upFace raw_)
        (GH.encodeField #copyright copyright raw_)
        (GH.encodeField #pubdate pubdate raw_)
        (GH.encodeField #duration duration raw_)
        (GH.encodeField #page page raw_)
        (GH.encodeField #cover cover raw_)
        (GH.encodeField #desc desc raw_)
        (GH.encodeField #tags tags raw_)
        (GH.encodeField #isExamined isExamined raw_)
        (GH.encodeField #ranks ranks raw_)
        (GH.encodeField #isRepublish isRepublish raw_)
        (GH.encodeField #staffInfo staffInfo raw_)
        (Std_.pure ())
        )
instance (GH.HasField "avid" GH.Slot Cvse'RecordingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "bvid" GH.Slot Cvse'RecordingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "title" GH.Slot Cvse'RecordingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "uploader" GH.Slot Cvse'RecordingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "upFace" GH.Slot Cvse'RecordingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 4)
instance (GH.HasField "copyright" GH.Slot Cvse'RecordingNewEntry Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "pubdate" GH.Slot Cvse'RecordingNewEntry Cvse'Time) where
    fieldByLabel  = (GH.ptrField 5)
instance (GH.HasField "duration" GH.Slot Cvse'RecordingNewEntry Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
instance (GH.HasField "page" GH.Slot Cvse'RecordingNewEntry Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
instance (GH.HasField "cover" GH.Slot Cvse'RecordingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 6)
instance (GH.HasField "desc" GH.Slot Cvse'RecordingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 7)
instance (GH.HasField "tags" GH.Slot Cvse'RecordingNewEntry (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 8)
instance (GH.HasField "isExamined" GH.Slot Cvse'RecordingNewEntry Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 1 1 0)
instance (GH.HasField "ranks" GH.Slot Cvse'RecordingNewEntry (R.List Cvse'Rank)) where
    fieldByLabel  = (GH.ptrField 9)
instance (GH.HasField "isRepublish" GH.Slot Cvse'RecordingNewEntry Std_.Bool) where
    fieldByLabel  = (GH.dataField 33 1 1 0)
instance (GH.HasField "staffInfo" GH.Slot Cvse'RecordingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 10)
data Cvse'RecordingDataEntry 
type instance (R.ReprFor Cvse'RecordingDataEntry) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'RecordingDataEntry) where
    typeId  = 17131781730824755557
instance (C.TypedStruct Cvse'RecordingDataEntry) where
    numStructWords  = 7
    numStructPtrs  = 3
instance (C.Allocate Cvse'RecordingDataEntry) where
    type AllocHint Cvse'RecordingDataEntry = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'RecordingDataEntry (C.Parsed Cvse'RecordingDataEntry))
instance (C.AllocateList Cvse'RecordingDataEntry) where
    type ListAllocHint Cvse'RecordingDataEntry = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'RecordingDataEntry (C.Parsed Cvse'RecordingDataEntry))
data instance C.Parsed Cvse'RecordingDataEntry
    = Cvse'RecordingDataEntry 
        {avid :: (RP.Parsed Basics.Text)
        ,bvid :: (RP.Parsed Basics.Text)
        ,view :: (RP.Parsed Std_.Int64)
        ,favorite :: (RP.Parsed Std_.Int64)
        ,coin :: (RP.Parsed Std_.Int64)
        ,like :: (RP.Parsed Std_.Int64)
        ,danmaku :: (RP.Parsed Std_.Int64)
        ,reply :: (RP.Parsed Std_.Int64)
        ,share :: (RP.Parsed Std_.Int64)
        ,date :: (RP.Parsed Cvse'Time)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'RecordingDataEntry))
deriving instance (Std_.Eq (C.Parsed Cvse'RecordingDataEntry))
instance (C.Parse Cvse'RecordingDataEntry (C.Parsed Cvse'RecordingDataEntry)) where
    parse raw_ = (Cvse'RecordingDataEntry <$> (GH.parseField #avid raw_)
                                          <*> (GH.parseField #bvid raw_)
                                          <*> (GH.parseField #view raw_)
                                          <*> (GH.parseField #favorite raw_)
                                          <*> (GH.parseField #coin raw_)
                                          <*> (GH.parseField #like raw_)
                                          <*> (GH.parseField #danmaku raw_)
                                          <*> (GH.parseField #reply raw_)
                                          <*> (GH.parseField #share raw_)
                                          <*> (GH.parseField #date raw_))
instance (C.Marshal Cvse'RecordingDataEntry (C.Parsed Cvse'RecordingDataEntry)) where
    marshalInto raw_ Cvse'RecordingDataEntry{..} = (do
        (GH.encodeField #avid avid raw_)
        (GH.encodeField #bvid bvid raw_)
        (GH.encodeField #view view raw_)
        (GH.encodeField #favorite favorite raw_)
        (GH.encodeField #coin coin raw_)
        (GH.encodeField #like like raw_)
        (GH.encodeField #danmaku danmaku raw_)
        (GH.encodeField #reply reply raw_)
        (GH.encodeField #share share raw_)
        (GH.encodeField #date date raw_)
        (Std_.pure ())
        )
instance (GH.HasField "avid" GH.Slot Cvse'RecordingDataEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "bvid" GH.Slot Cvse'RecordingDataEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "view" GH.Slot Cvse'RecordingDataEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "favorite" GH.Slot Cvse'RecordingDataEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "coin" GH.Slot Cvse'RecordingDataEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "like" GH.Slot Cvse'RecordingDataEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 3 64 0)
instance (GH.HasField "danmaku" GH.Slot Cvse'RecordingDataEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 4 64 0)
instance (GH.HasField "reply" GH.Slot Cvse'RecordingDataEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 5 64 0)
instance (GH.HasField "share" GH.Slot Cvse'RecordingDataEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 6 64 0)
instance (GH.HasField "date" GH.Slot Cvse'RecordingDataEntry Cvse'Time) where
    fieldByLabel  = (GH.ptrField 2)
data Cvse'Index 
type instance (R.ReprFor Cvse'Index) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'Index) where
    typeId  = 10423373514169926852
instance (C.TypedStruct Cvse'Index) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Cvse'Index) where
    type AllocHint Cvse'Index = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'Index (C.Parsed Cvse'Index))
instance (C.AllocateList Cvse'Index) where
    type ListAllocHint Cvse'Index = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'Index (C.Parsed Cvse'Index))
data instance C.Parsed Cvse'Index
    = Cvse'Index 
        {avid :: (RP.Parsed Basics.Text)
        ,bvid :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'Index))
deriving instance (Std_.Eq (C.Parsed Cvse'Index))
instance (C.Parse Cvse'Index (C.Parsed Cvse'Index)) where
    parse raw_ = (Cvse'Index <$> (GH.parseField #avid raw_)
                             <*> (GH.parseField #bvid raw_))
instance (C.Marshal Cvse'Index (C.Parsed Cvse'Index)) where
    marshalInto raw_ Cvse'Index{..} = (do
        (GH.encodeField #avid avid raw_)
        (GH.encodeField #bvid bvid raw_)
        (Std_.pure ())
        )
instance (GH.HasField "avid" GH.Slot Cvse'Index Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "bvid" GH.Slot Cvse'Index Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data Cvse'RankingInfoEntry 
type instance (R.ReprFor Cvse'RankingInfoEntry) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'RankingInfoEntry) where
    typeId  = 16290349068181940150
instance (C.TypedStruct Cvse'RankingInfoEntry) where
    numStructWords  = 18
    numStructPtrs  = 4
instance (C.Allocate Cvse'RankingInfoEntry) where
    type AllocHint Cvse'RankingInfoEntry = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'RankingInfoEntry (C.Parsed Cvse'RankingInfoEntry))
instance (C.AllocateList Cvse'RankingInfoEntry) where
    type ListAllocHint Cvse'RankingInfoEntry = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'RankingInfoEntry (C.Parsed Cvse'RankingInfoEntry))
data instance C.Parsed Cvse'RankingInfoEntry
    = Cvse'RankingInfoEntry 
        {avid :: (RP.Parsed Basics.Text)
        ,bvid :: (RP.Parsed Basics.Text)
        ,prev :: (RP.Parsed Cvse'RecordingDataEntry)
        ,curr :: (RP.Parsed Cvse'RecordingDataEntry)
        ,isNew :: (RP.Parsed Std_.Bool)
        ,view :: (RP.Parsed Std_.Int64)
        ,like :: (RP.Parsed Std_.Int64)
        ,share :: (RP.Parsed Std_.Int64)
        ,favorite :: (RP.Parsed Std_.Int64)
        ,coin :: (RP.Parsed Std_.Int64)
        ,reply :: (RP.Parsed Std_.Int64)
        ,danmaku :: (RP.Parsed Std_.Int64)
        ,pointA :: (RP.Parsed Std_.Double)
        ,pointB :: (RP.Parsed Std_.Double)
        ,pointC :: (RP.Parsed Std_.Double)
        ,fixA :: (RP.Parsed Std_.Double)
        ,fixB :: (RP.Parsed Std_.Double)
        ,fixC :: (RP.Parsed Std_.Double)
        ,scoreA :: (RP.Parsed Std_.Double)
        ,scoreB :: (RP.Parsed Std_.Double)
        ,scoreC :: (RP.Parsed Std_.Double)
        ,totalScore :: (RP.Parsed Std_.Double)
        ,rank :: (RP.Parsed Std_.Int32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'RankingInfoEntry))
deriving instance (Std_.Eq (C.Parsed Cvse'RankingInfoEntry))
instance (C.Parse Cvse'RankingInfoEntry (C.Parsed Cvse'RankingInfoEntry)) where
    parse raw_ = (Cvse'RankingInfoEntry <$> (GH.parseField #avid raw_)
                                        <*> (GH.parseField #bvid raw_)
                                        <*> (GH.parseField #prev raw_)
                                        <*> (GH.parseField #curr raw_)
                                        <*> (GH.parseField #isNew raw_)
                                        <*> (GH.parseField #view raw_)
                                        <*> (GH.parseField #like raw_)
                                        <*> (GH.parseField #share raw_)
                                        <*> (GH.parseField #favorite raw_)
                                        <*> (GH.parseField #coin raw_)
                                        <*> (GH.parseField #reply raw_)
                                        <*> (GH.parseField #danmaku raw_)
                                        <*> (GH.parseField #pointA raw_)
                                        <*> (GH.parseField #pointB raw_)
                                        <*> (GH.parseField #pointC raw_)
                                        <*> (GH.parseField #fixA raw_)
                                        <*> (GH.parseField #fixB raw_)
                                        <*> (GH.parseField #fixC raw_)
                                        <*> (GH.parseField #scoreA raw_)
                                        <*> (GH.parseField #scoreB raw_)
                                        <*> (GH.parseField #scoreC raw_)
                                        <*> (GH.parseField #totalScore raw_)
                                        <*> (GH.parseField #rank raw_))
instance (C.Marshal Cvse'RankingInfoEntry (C.Parsed Cvse'RankingInfoEntry)) where
    marshalInto raw_ Cvse'RankingInfoEntry{..} = (do
        (GH.encodeField #avid avid raw_)
        (GH.encodeField #bvid bvid raw_)
        (GH.encodeField #prev prev raw_)
        (GH.encodeField #curr curr raw_)
        (GH.encodeField #isNew isNew raw_)
        (GH.encodeField #view view raw_)
        (GH.encodeField #like like raw_)
        (GH.encodeField #share share raw_)
        (GH.encodeField #favorite favorite raw_)
        (GH.encodeField #coin coin raw_)
        (GH.encodeField #reply reply raw_)
        (GH.encodeField #danmaku danmaku raw_)
        (GH.encodeField #pointA pointA raw_)
        (GH.encodeField #pointB pointB raw_)
        (GH.encodeField #pointC pointC raw_)
        (GH.encodeField #fixA fixA raw_)
        (GH.encodeField #fixB fixB raw_)
        (GH.encodeField #fixC fixC raw_)
        (GH.encodeField #scoreA scoreA raw_)
        (GH.encodeField #scoreB scoreB raw_)
        (GH.encodeField #scoreC scoreC raw_)
        (GH.encodeField #totalScore totalScore raw_)
        (GH.encodeField #rank rank raw_)
        (Std_.pure ())
        )
instance (GH.HasField "avid" GH.Slot Cvse'RankingInfoEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "bvid" GH.Slot Cvse'RankingInfoEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "prev" GH.Slot Cvse'RankingInfoEntry Cvse'RecordingDataEntry) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "curr" GH.Slot Cvse'RankingInfoEntry Cvse'RecordingDataEntry) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "isNew" GH.Slot Cvse'RankingInfoEntry Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "view" GH.Slot Cvse'RankingInfoEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "like" GH.Slot Cvse'RankingInfoEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "share" GH.Slot Cvse'RankingInfoEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 3 64 0)
instance (GH.HasField "favorite" GH.Slot Cvse'RankingInfoEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 4 64 0)
instance (GH.HasField "coin" GH.Slot Cvse'RankingInfoEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 5 64 0)
instance (GH.HasField "reply" GH.Slot Cvse'RankingInfoEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 6 64 0)
instance (GH.HasField "danmaku" GH.Slot Cvse'RankingInfoEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 7 64 0)
instance (GH.HasField "pointA" GH.Slot Cvse'RankingInfoEntry Std_.Double) where
    fieldByLabel  = (GH.dataField 0 8 64 0)
instance (GH.HasField "pointB" GH.Slot Cvse'RankingInfoEntry Std_.Double) where
    fieldByLabel  = (GH.dataField 0 9 64 0)
instance (GH.HasField "pointC" GH.Slot Cvse'RankingInfoEntry Std_.Double) where
    fieldByLabel  = (GH.dataField 0 10 64 0)
instance (GH.HasField "fixA" GH.Slot Cvse'RankingInfoEntry Std_.Double) where
    fieldByLabel  = (GH.dataField 0 11 64 0)
instance (GH.HasField "fixB" GH.Slot Cvse'RankingInfoEntry Std_.Double) where
    fieldByLabel  = (GH.dataField 0 12 64 0)
instance (GH.HasField "fixC" GH.Slot Cvse'RankingInfoEntry Std_.Double) where
    fieldByLabel  = (GH.dataField 0 13 64 0)
instance (GH.HasField "scoreA" GH.Slot Cvse'RankingInfoEntry Std_.Double) where
    fieldByLabel  = (GH.dataField 0 14 64 0)
instance (GH.HasField "scoreB" GH.Slot Cvse'RankingInfoEntry Std_.Double) where
    fieldByLabel  = (GH.dataField 0 15 64 0)
instance (GH.HasField "scoreC" GH.Slot Cvse'RankingInfoEntry Std_.Double) where
    fieldByLabel  = (GH.dataField 0 16 64 0)
instance (GH.HasField "totalScore" GH.Slot Cvse'RankingInfoEntry Std_.Double) where
    fieldByLabel  = (GH.dataField 0 17 64 0)
instance (GH.HasField "rank" GH.Slot Cvse'RankingInfoEntry Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)