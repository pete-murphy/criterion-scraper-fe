module Foreign.Hooks where

import Prelude

import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Effect.Uncurried (EffectFn2)
import Effect.Uncurried as Uncurried
import React.Basic.Hooks (Hook)
import React.Basic.Hooks as Hooks

useDebounce
  :: forall a
   . a
  -> Milliseconds
  -> Hook UseDebounce a

useDebounce value (Milliseconds milliseconds) =
  Hooks.unsafeHook
    ( Uncurried.runEffectFn2
        useDebounce_
        value
        milliseconds
    )

newtype UseDebounce hooks = UseDebounce hooks

derive instance Newtype (UseDebounce hooks) _

foreign import useDebounce_
  :: forall a
   . EffectFn2
       a
       Number
       a