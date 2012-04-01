{-# LANGUAGE
    ConstraintKinds
  , DataKinds
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , KindSignatures
  , MultiParamTypeClasses
  , OverlappingInstances
  , TypeOperators
  , UndecidableInstances #-}
module Data.Record.List
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

infixr 5 `Cons`
data Record (fields :: [(*, *)]) where
  Nil :: Record '[]
  Cons :: Field label value -> !(Record fields) -> Record ('(label, value) ': fields)

newtype Field label value = Field value

mkField :: label -> value -> Field label value
mkField _label value = Field value

empty :: Record '[]
empty = Nil

extend :: label -> value -> Record fields -> Record ('(label, value) ': fields)
extend label value record = mkField label value `Cons` record

class Elem '(label, value) fields True =>
      Select label value fields | label fields -> value where
  select :: label -> Record fields -> value

instance Select label value ('(label, value) ': fields) where
  select _label (Field value `Cons` _record) = value

instance Select label value fields =>
         Select label value (field ': fields) where
  select label (_field `Cons` record) = select label record

class Remove label fields fields' | label fields -> fields' where
  remove :: label -> Record fields -> Record fields'

instance Remove label '[] '[] where
  remove _label Nil = Nil

instance Remove label ('(label, value) ': fields) fields where
  remove _label (_field `Cons` record) = record

instance ( Remove label fields fields'
         , fields'' ~ (field ': fields')
         ) =>
         Remove label (field ': fields) fields'' where
  remove label (field `Cons` record) = field `Cons` remove label record

class Elem (field :: (*, *)) (fields :: [(*, *)]) (result :: Bool) | field fields -> result
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
(#|) :: (label, value) -> Record fields -> Record ('(label, value) ': fields)
(label, value) #| record = extend label value record

(#.) :: Select label value fields => Record fields -> label -> value
(#.) = flip select

(#-) :: Remove label fields fields' => Record fields -> label -> Record fields'
(#-) = flip remove
