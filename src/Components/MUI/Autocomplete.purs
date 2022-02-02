module Components.MUI.Autocomplete
  ( mkAutocomplete
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as Uncurried
import React.Basic as Basic
import React.Basic.Hooks (Component)
import React.Basic.Hooks as Hooks

type Props =
  { options :: Array String
  , value :: Array String
  , onChange :: Array String -> Effect Unit
  , label :: String
  }

mkAutocomplete :: Component Props
mkAutocomplete = do
  Hooks.component "Autocomplete" \props -> Hooks.do
    pure
      ( Basic.element
          autocomplete_
          { options: props.options
          , value: props.value
          , onChange: Uncurried.mkEffectFn1 props.onChange
          , label: props.label
          }
      )

foreign import autocomplete_
  :: Basic.ReactComponent
       { options :: Array String
       , value :: Array String
       , onChange :: EffectFn1 (Array String) Unit
       , label :: String
       }