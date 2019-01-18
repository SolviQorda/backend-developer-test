module Halogen.Query.HalogenQ where

import Prelude

import Data.Bifunctor (class Bifunctor)

data HalogenQ f act i a
  = Initialize a
  | Finalize a
  | Receive i a
  | Handle act a
  | Request (f a)

instance bifunctorHalogenQ :: Functor f => Bifunctor (HalogenQ f act) where
  bimap f g = case _ of
    Initialize a -> Initialize (g a)
    Finalize a -> Finalize (g a)
    Receive i a -> Receive (f i) (g a)
    Handle act a -> Handle act (g a)
    Request fa -> Request (map g fa)

derive instance functorHalogenQ :: Functor f => Functor (HalogenQ f act i)
