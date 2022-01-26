module Main where

import Prelude

import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception as Exception
import React.Basic.DOM as DOM
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

main :: Effect Unit
main = do
  maybeRoot <- HTML.window
    >>= Window.document
    >>= HTMLDocument.toNonElementParentNode
      >>> NonElementParentNode.getElementById "root"
  case maybeRoot of
    Nothing -> Exception.throw "Root element not found."
    Just root -> do
      app <- App.mkApp
      DOM.render (app unit) root
