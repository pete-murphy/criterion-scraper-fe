module Foreign.Hooks
  ( UseIntersectionObserver
  , useIntersectionObserver
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as Uncurried
import React.Basic (Ref)
import React.Basic.Hooks (Hook)
import React.Basic.Hooks as Hooks
import Web.DOM (Node)

newtype UseIntersectionObserver hooks = UseIntersectionObserver hooks

derive instance Newtype (UseIntersectionObserver hooks) _

foreign import useIntersectionObserver_
  :: EffectFn1
       (Ref (Nullable Node))
       Boolean

useIntersectionObserver
  :: Ref (Nullable Node)
  -> Hook UseIntersectionObserver Boolean
useIntersectionObserver ref =
  Hooks.unsafeHook
    ( Uncurried.runEffectFn1
        useIntersectionObserver_
        ref
    )

-- |
-- foreign import useIntersectionObserver_
--   :: EffectFn2
--        (Ref (Nullable Node))
--        (EffectFn1 Boolean (Effect Unit))
--        Unit

-- useIntersectionObserver
--   :: Ref (Nullable Node)
--   -> (Boolean -> Effect (Effect Unit))
--   -> Hook UseIntersectionObserver Unit
-- useIntersectionObserver ref withIntersection =
--   Hooks.unsafeHook
--     ( Uncurried.runEffectFn2
--         useIntersectionObserver_
--         ref
--         (Uncurried.mkEffectFn1 withIntersection)
--     )