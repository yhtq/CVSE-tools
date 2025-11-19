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
                                                                      ,(GH.toUntypedMethodHandler ((cvse'updateAddressingDataEntry) s_))] [])
class (Cvse'server_ s_) where
    {-# MINIMAL cvse'updateModifyEntry,cvse'updateNewEntry,cvse'updateAddressingDataEntry #-}
    cvse'updateModifyEntry :: s_ -> (GH.MethodHandler Cvse'updateModifyEntry'params Cvse'updateModifyEntry'results)
    cvse'updateModifyEntry _ = GH.methodUnimplemented
    cvse'updateNewEntry :: s_ -> (GH.MethodHandler Cvse'updateNewEntry'params Cvse'updateNewEntry'results)
    cvse'updateNewEntry _ = GH.methodUnimplemented
    cvse'updateAddressingDataEntry :: s_ -> (GH.MethodHandler Cvse'updateAddressingDataEntry'params Cvse'updateAddressingDataEntry'results)
    cvse'updateAddressingDataEntry _ = GH.methodUnimplemented
instance (GH.HasMethod "updateModifyEntry" Cvse Cvse'updateModifyEntry'params Cvse'updateModifyEntry'results) where
    methodByLabel  = (GH.Method 11881061825597523281 0)
instance (GH.HasMethod "updateNewEntry" Cvse Cvse'updateNewEntry'params Cvse'updateNewEntry'results) where
    methodByLabel  = (GH.Method 11881061825597523281 1)
instance (GH.HasMethod "updateAddressingDataEntry" Cvse Cvse'updateAddressingDataEntry'params Cvse'updateAddressingDataEntry'results) where
    methodByLabel  = (GH.Method 11881061825597523281 2)
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
    numStructWords  = 0
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
        {entries :: (RP.Parsed (R.List Cvse'AddressingNewEntry))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'updateNewEntry'params))
deriving instance (Std_.Eq (C.Parsed Cvse'updateNewEntry'params))
instance (C.Parse Cvse'updateNewEntry'params (C.Parsed Cvse'updateNewEntry'params)) where
    parse raw_ = (Cvse'updateNewEntry'params <$> (GH.parseField #entries raw_))
instance (C.Marshal Cvse'updateNewEntry'params (C.Parsed Cvse'updateNewEntry'params)) where
    marshalInto raw_ Cvse'updateNewEntry'params{..} = (do
        (GH.encodeField #entries entries raw_)
        (Std_.pure ())
        )
instance (GH.HasField "entries" GH.Slot Cvse'updateNewEntry'params (R.List Cvse'AddressingNewEntry)) where
    fieldByLabel  = (GH.ptrField 0)
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
data Cvse'updateAddressingDataEntry'params 
type instance (R.ReprFor Cvse'updateAddressingDataEntry'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'updateAddressingDataEntry'params) where
    typeId  = 18402389460281104284
instance (C.TypedStruct Cvse'updateAddressingDataEntry'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Cvse'updateAddressingDataEntry'params) where
    type AllocHint Cvse'updateAddressingDataEntry'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'updateAddressingDataEntry'params (C.Parsed Cvse'updateAddressingDataEntry'params))
instance (C.AllocateList Cvse'updateAddressingDataEntry'params) where
    type ListAllocHint Cvse'updateAddressingDataEntry'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'updateAddressingDataEntry'params (C.Parsed Cvse'updateAddressingDataEntry'params))
data instance C.Parsed Cvse'updateAddressingDataEntry'params
    = Cvse'updateAddressingDataEntry'params 
        {entries :: (RP.Parsed (R.List Cvse'AddressingDataEntry))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'updateAddressingDataEntry'params))
deriving instance (Std_.Eq (C.Parsed Cvse'updateAddressingDataEntry'params))
instance (C.Parse Cvse'updateAddressingDataEntry'params (C.Parsed Cvse'updateAddressingDataEntry'params)) where
    parse raw_ = (Cvse'updateAddressingDataEntry'params <$> (GH.parseField #entries raw_))
instance (C.Marshal Cvse'updateAddressingDataEntry'params (C.Parsed Cvse'updateAddressingDataEntry'params)) where
    marshalInto raw_ Cvse'updateAddressingDataEntry'params{..} = (do
        (GH.encodeField #entries entries raw_)
        (Std_.pure ())
        )
instance (GH.HasField "entries" GH.Slot Cvse'updateAddressingDataEntry'params (R.List Cvse'AddressingDataEntry)) where
    fieldByLabel  = (GH.ptrField 0)
data Cvse'updateAddressingDataEntry'results 
type instance (R.ReprFor Cvse'updateAddressingDataEntry'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'updateAddressingDataEntry'results) where
    typeId  = 16742526759880192168
instance (C.TypedStruct Cvse'updateAddressingDataEntry'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Cvse'updateAddressingDataEntry'results) where
    type AllocHint Cvse'updateAddressingDataEntry'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'updateAddressingDataEntry'results (C.Parsed Cvse'updateAddressingDataEntry'results))
instance (C.AllocateList Cvse'updateAddressingDataEntry'results) where
    type ListAllocHint Cvse'updateAddressingDataEntry'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'updateAddressingDataEntry'results (C.Parsed Cvse'updateAddressingDataEntry'results))
data instance C.Parsed Cvse'updateAddressingDataEntry'results
    = Cvse'updateAddressingDataEntry'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'updateAddressingDataEntry'results))
deriving instance (Std_.Eq (C.Parsed Cvse'updateAddressingDataEntry'results))
instance (C.Parse Cvse'updateAddressingDataEntry'results (C.Parsed Cvse'updateAddressingDataEntry'results)) where
    parse raw_ = (Std_.pure Cvse'updateAddressingDataEntry'results)
instance (C.Marshal Cvse'updateAddressingDataEntry'results (C.Parsed Cvse'updateAddressingDataEntry'results)) where
    marshalInto _raw (Cvse'updateAddressingDataEntry'results) = (Std_.pure ())
data Cvse'DayDate 
type instance (R.ReprFor Cvse'DayDate) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'DayDate) where
    typeId  = 14557538036996046821
instance (C.TypedStruct Cvse'DayDate) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Cvse'DayDate) where
    type AllocHint Cvse'DayDate = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'DayDate (C.Parsed Cvse'DayDate))
instance (C.AllocateList Cvse'DayDate) where
    type ListAllocHint Cvse'DayDate = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'DayDate (C.Parsed Cvse'DayDate))
data instance C.Parsed Cvse'DayDate
    = Cvse'DayDate 
        {year :: (RP.Parsed Std_.Int16)
        ,month :: (RP.Parsed Std_.Int8)
        ,day :: (RP.Parsed Std_.Int8)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'DayDate))
deriving instance (Std_.Eq (C.Parsed Cvse'DayDate))
instance (C.Parse Cvse'DayDate (C.Parsed Cvse'DayDate)) where
    parse raw_ = (Cvse'DayDate <$> (GH.parseField #year raw_)
                               <*> (GH.parseField #month raw_)
                               <*> (GH.parseField #day raw_))
instance (C.Marshal Cvse'DayDate (C.Parsed Cvse'DayDate)) where
    marshalInto raw_ Cvse'DayDate{..} = (do
        (GH.encodeField #year year raw_)
        (GH.encodeField #month month raw_)
        (GH.encodeField #day day raw_)
        (Std_.pure ())
        )
instance (GH.HasField "year" GH.Slot Cvse'DayDate Std_.Int16) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "month" GH.Slot Cvse'DayDate Std_.Int8) where
    fieldByLabel  = (GH.dataField 16 0 8 0)
instance (GH.HasField "day" GH.Slot Cvse'DayDate Std_.Int8) where
    fieldByLabel  = (GH.dataField 24 0 8 0)
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
        ,ranks :: (RP.Parsed (R.List Cvse'Rank))
        ,isRepublish :: (RP.Parsed Std_.Bool)
        ,staffInfo :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'ModifyEntry))
deriving instance (Std_.Eq (C.Parsed Cvse'ModifyEntry))
instance (C.Parse Cvse'ModifyEntry (C.Parsed Cvse'ModifyEntry)) where
    parse raw_ = (Cvse'ModifyEntry <$> (GH.parseField #avid raw_)
                                   <*> (GH.parseField #bvid raw_)
                                   <*> (GH.parseField #ranks raw_)
                                   <*> (GH.parseField #isRepublish raw_)
                                   <*> (GH.parseField #staffInfo raw_))
instance (C.Marshal Cvse'ModifyEntry (C.Parsed Cvse'ModifyEntry)) where
    marshalInto raw_ Cvse'ModifyEntry{..} = (do
        (GH.encodeField #avid avid raw_)
        (GH.encodeField #bvid bvid raw_)
        (GH.encodeField #ranks ranks raw_)
        (GH.encodeField #isRepublish isRepublish raw_)
        (GH.encodeField #staffInfo staffInfo raw_)
        (Std_.pure ())
        )
instance (GH.HasField "avid" GH.Slot Cvse'ModifyEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "bvid" GH.Slot Cvse'ModifyEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "ranks" GH.Slot Cvse'ModifyEntry (R.List Cvse'Rank)) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "isRepublish" GH.Slot Cvse'ModifyEntry Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "staffInfo" GH.Slot Cvse'ModifyEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
data Cvse'AddressingNewEntry 
type instance (R.ReprFor Cvse'AddressingNewEntry) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'AddressingNewEntry) where
    typeId  = 16118650743386912332
instance (C.TypedStruct Cvse'AddressingNewEntry) where
    numStructWords  = 2
    numStructPtrs  = 9
instance (C.Allocate Cvse'AddressingNewEntry) where
    type AllocHint Cvse'AddressingNewEntry = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'AddressingNewEntry (C.Parsed Cvse'AddressingNewEntry))
instance (C.AllocateList Cvse'AddressingNewEntry) where
    type ListAllocHint Cvse'AddressingNewEntry = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'AddressingNewEntry (C.Parsed Cvse'AddressingNewEntry))
data instance C.Parsed Cvse'AddressingNewEntry
    = Cvse'AddressingNewEntry 
        {avid :: (RP.Parsed Basics.Text)
        ,bvid :: (RP.Parsed Basics.Text)
        ,title :: (RP.Parsed Basics.Text)
        ,uploader :: (RP.Parsed Basics.Text)
        ,upFace :: (RP.Parsed Basics.Text)
        ,copyright :: (RP.Parsed Std_.Int32)
        ,pubdate :: (RP.Parsed Basics.Text)
        ,duration :: (RP.Parsed Std_.Int32)
        ,page :: (RP.Parsed Std_.Int32)
        ,cover :: (RP.Parsed Basics.Text)
        ,desc :: (RP.Parsed Basics.Text)
        ,tags :: (RP.Parsed (R.List Basics.Text))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'AddressingNewEntry))
deriving instance (Std_.Eq (C.Parsed Cvse'AddressingNewEntry))
instance (C.Parse Cvse'AddressingNewEntry (C.Parsed Cvse'AddressingNewEntry)) where
    parse raw_ = (Cvse'AddressingNewEntry <$> (GH.parseField #avid raw_)
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
                                          <*> (GH.parseField #tags raw_))
instance (C.Marshal Cvse'AddressingNewEntry (C.Parsed Cvse'AddressingNewEntry)) where
    marshalInto raw_ Cvse'AddressingNewEntry{..} = (do
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
        (Std_.pure ())
        )
instance (GH.HasField "avid" GH.Slot Cvse'AddressingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "bvid" GH.Slot Cvse'AddressingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "title" GH.Slot Cvse'AddressingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "uploader" GH.Slot Cvse'AddressingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "upFace" GH.Slot Cvse'AddressingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 4)
instance (GH.HasField "copyright" GH.Slot Cvse'AddressingNewEntry Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "pubdate" GH.Slot Cvse'AddressingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 5)
instance (GH.HasField "duration" GH.Slot Cvse'AddressingNewEntry Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
instance (GH.HasField "page" GH.Slot Cvse'AddressingNewEntry Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
instance (GH.HasField "cover" GH.Slot Cvse'AddressingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 6)
instance (GH.HasField "desc" GH.Slot Cvse'AddressingNewEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 7)
instance (GH.HasField "tags" GH.Slot Cvse'AddressingNewEntry (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 8)
data Cvse'AddressingDataEntry 
type instance (R.ReprFor Cvse'AddressingDataEntry) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Cvse'AddressingDataEntry) where
    typeId  = 9429155103964698714
instance (C.TypedStruct Cvse'AddressingDataEntry) where
    numStructWords  = 7
    numStructPtrs  = 3
instance (C.Allocate Cvse'AddressingDataEntry) where
    type AllocHint Cvse'AddressingDataEntry = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Cvse'AddressingDataEntry (C.Parsed Cvse'AddressingDataEntry))
instance (C.AllocateList Cvse'AddressingDataEntry) where
    type ListAllocHint Cvse'AddressingDataEntry = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Cvse'AddressingDataEntry (C.Parsed Cvse'AddressingDataEntry))
data instance C.Parsed Cvse'AddressingDataEntry
    = Cvse'AddressingDataEntry 
        {avid :: (RP.Parsed Basics.Text)
        ,bvid :: (RP.Parsed Basics.Text)
        ,view :: (RP.Parsed Std_.Int64)
        ,favorite :: (RP.Parsed Std_.Int64)
        ,coin :: (RP.Parsed Std_.Int64)
        ,like :: (RP.Parsed Std_.Int64)
        ,danmaku :: (RP.Parsed Std_.Int64)
        ,reply :: (RP.Parsed Std_.Int64)
        ,share :: (RP.Parsed Std_.Int64)
        ,date :: (RP.Parsed Cvse'DayDate)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Cvse'AddressingDataEntry))
deriving instance (Std_.Eq (C.Parsed Cvse'AddressingDataEntry))
instance (C.Parse Cvse'AddressingDataEntry (C.Parsed Cvse'AddressingDataEntry)) where
    parse raw_ = (Cvse'AddressingDataEntry <$> (GH.parseField #avid raw_)
                                           <*> (GH.parseField #bvid raw_)
                                           <*> (GH.parseField #view raw_)
                                           <*> (GH.parseField #favorite raw_)
                                           <*> (GH.parseField #coin raw_)
                                           <*> (GH.parseField #like raw_)
                                           <*> (GH.parseField #danmaku raw_)
                                           <*> (GH.parseField #reply raw_)
                                           <*> (GH.parseField #share raw_)
                                           <*> (GH.parseField #date raw_))
instance (C.Marshal Cvse'AddressingDataEntry (C.Parsed Cvse'AddressingDataEntry)) where
    marshalInto raw_ Cvse'AddressingDataEntry{..} = (do
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
instance (GH.HasField "avid" GH.Slot Cvse'AddressingDataEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "bvid" GH.Slot Cvse'AddressingDataEntry Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "view" GH.Slot Cvse'AddressingDataEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "favorite" GH.Slot Cvse'AddressingDataEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "coin" GH.Slot Cvse'AddressingDataEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "like" GH.Slot Cvse'AddressingDataEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 3 64 0)
instance (GH.HasField "danmaku" GH.Slot Cvse'AddressingDataEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 4 64 0)
instance (GH.HasField "reply" GH.Slot Cvse'AddressingDataEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 5 64 0)
instance (GH.HasField "share" GH.Slot Cvse'AddressingDataEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 6 64 0)
instance (GH.HasField "date" GH.Slot Cvse'AddressingDataEntry Cvse'DayDate) where
    fieldByLabel  = (GH.ptrField 2)