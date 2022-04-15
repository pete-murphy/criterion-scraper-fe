module Components.DragAndDropList
  ( mkDragAndDropList
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as Uncurried
import React.Basic (JSX)
import React.Basic as Basic
import React.Basic.Hooks (Component)
import React.Basic.Hooks as Hooks

type Props a =
  { items :: Array a
  , renderItem :: a -> JSX
  , keyForItem :: a -> String
  , setItems :: Array a -> Effect Unit
  , onClickItem :: a -> Effect Unit
  , isItemSelected :: a -> Boolean
  }

mkDragAndDropList :: forall a. Component (Props a)
mkDragAndDropList = do
  Hooks.component "Slider" \props -> Hooks.do
    pure
      ( Basic.element
          dragAndDropList_
          { items: props.items
          , renderItem: props.renderItem
          , keyForItem: props.keyForItem
          , setItems: Uncurried.mkEffectFn1 props.setItems
          , onClickItem: Uncurried.mkEffectFn1 props.onClickItem
          , isItemSelected: props.isItemSelected
          }
      )

foreign import dragAndDropList_
  :: forall a
   . Basic.ReactComponent
       { items :: Array a
       , renderItem :: a -> JSX
       , keyForItem :: a -> String
       , setItems :: EffectFn1 (Array a) Unit
       , onClickItem :: EffectFn1 a Unit
       , isItemSelected :: a -> Boolean
       }