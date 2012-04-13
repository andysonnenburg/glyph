{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , MultiParamTypeClasses
  , OverlappingInstances
  , TypeOperators
  , UndecidableInstances #-}
module Data.Record.Map
       ( Record
       , empty
       , extend
       , Select (..)
       , Remove (..)
       , Elem
       , Member
       , Lacks
       , (#=)
       , (#|)
       , (#.)
       , (#-)
       ) where

import Data.Hashable
import Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as Map
import Data.Typeable

import GHC.Exts (Any)
import Unsafe.Coerce as Unsafe

data Record (fields :: [(*, *)])
  = Record { unRecord :: Map Label Value
           }

empty :: Record '[]
empty = Record Map.empty

extend :: ( Typeable label
          , Lacks label fields
          ) => label -> value -> Record fields -> Record ('(label, value) ': fields)
extend label value = Record . Map.insert label' value' . unRecord
  where
    label' = toLabel label
    value' = toValue value

class ( Typeable label
      , Elem '(label, value) fields True
      ) =>
      Select label value fields | label fields -> value where
  select :: label -> Record fields -> value
  select label = fromValue . (!toLabel label) . unRecord

instance Typeable label => Select label value ('(label, value) ': fields)

instance ( Typeable label
         , Select label value fields
         ) =>
         Select label value (field ': fields)

class ( Typeable label
      , Lacks label fields'
      ) => Remove label fields fields' | label fields -> fields' where
  remove :: label -> Record fields -> Record fields'
  remove label = Record . Map.delete (toLabel label) . unRecord

instance Typeable label => Remove label '[] '[]

instance ( Typeable label
         , Lacks label fields
         ) => Remove label ('(label, value) ': fields) fields

instance ( Typeable label
         , Remove label fields fields'
         , fields'' ~ (field ': fields')
         ) =>
         Remove label (field ': fields) fields''

class Elem (field :: (*, *)) (fields :: [(*, *)]) (result :: Bool)
  | field fields -> result
instance Elem field '[] False
instance Elem field (field ': fields) True
instance Elem field fields result => Elem field (field' ': fields) result

class Member label (fields :: [(*, *)]) (result :: Bool) | label fields -> result
instance Member label '[] False
instance Member label ('(label, value) ': fields) True
instance Member label fields result => Member label (field ': fields) result

type Lacks label fields = Member label fields False

infixr 6 #=
(#=) :: label -> value -> (label, value)
label #= value = (label, value)

infixr 5 #|
(#|) :: ( Typeable label
        , Lacks label fields
        ) => (label, value) -> Record fields -> Record ('(label, value) ': fields)
(label, value) #| record = extend label value record

(#.) :: Select label value fields => Record fields -> label -> value
(#.) = flip select

(#-) :: Remove label fields fields' => Record fields -> label -> Record fields'
(#-) = flip remove

type Map = HashMap

newtype Label = Label TypeRep deriving (Eq, Hashable)

toLabel :: Typeable label => label -> Label
toLabel = Label . typeOf

newtype Value = Value { unValue :: Any }

toValue :: value -> Value
toValue = Value . Unsafe.unsafeCoerce

fromValue :: Value -> value
fromValue = Unsafe.unsafeCoerce . unValue
