module Components.MUI.Slider
  ( mkSlider
  , Range
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as Uncurried
import React.Basic as Basic
import React.Basic.Hooks (Component)
import React.Basic.Hooks as Hooks

type Range =
  { min :: Int
  , max :: Int
  }

type Props =
  { thumbs :: Range
  , setThumbs :: Range -> Effect Unit
  , minDistance :: Int
  , bounds :: Range
  }

mkSlider :: Component Props
mkSlider = do
  Hooks.component "Slider" \props -> Hooks.do
    pure
      ( Basic.element
          slider_
          { value: [ props.thumbs.min, props.thumbs.max ]
          , setValue: Uncurried.mkEffectFn1 \ns -> case ns of
              [ min, max ] -> props.setThumbs { min, max }
              _ -> pure unit
          , minDistance: props.minDistance
          , min: props.bounds.min
          , max: props.bounds.max
          }
      )

foreign import slider_
  :: Basic.ReactComponent
       { value :: Array Int
       , setValue :: EffectFn1 (Array Int) Unit
       , minDistance :: Int
       , min :: Int
       , max :: Int
       }