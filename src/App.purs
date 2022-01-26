module App
  ( mkApp
  ) where

import Prelude

import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Control.Monad.Except as ExceptT
import Data.Argonaut as Argonaut
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Effect.Aff (Aff)
import Model.DateTime (DateTime)
import React.Basic.DOM as DOM
import React.Basic.Events as Events
import React.Basic.Hooks (Component, (/\))
import React.Basic.Hooks as Hooks
import React.Basic.Hooks.Aff as Hooks.Aff

type Movie =
  { title :: String
  , year :: Int
  , director :: String
  , thumbnailURL :: String
  , detailsURL :: String
  , movieId :: String
  , country :: String
  , created :: DateTime
  , modified :: DateTime
  }

data AppError
  = FetchError Affjax.Error
  | DecodeError Argonaut.JsonDecodeError

instance Show AppError where
  show = renderAppError

renderAppError :: AppError -> String
renderAppError = case _ of
  FetchError err -> Affjax.printError err
  DecodeError err -> show err

fetchMovies :: ExceptT AppError Aff (Array Movie)
fetchMovies = do
  let url = "http://167.99.230.0:8080/movies"
  response <- ExceptT.withExceptT FetchError
    (ExceptT (Affjax.get ResponseFormat.json url))
  ExceptT.withExceptT DecodeError
    (Except.except (Argonaut.decodeJson response.body))

mkApp :: Component Unit
mkApp = do
  movieList <- mkMovieList
  Hooks.component "App" \_ -> Hooks.do
    result <- Hooks.Aff.useAff unit do
      Except.runExceptT fetchMovies
    pure case result of
      Nothing -> mempty
      Just result' -> case result' of
        Left appError -> DOM.text (show appError)
        Right movies -> movieList movies

mkMovieList :: Component (Array Movie)
mkMovieList = do
  movieListItem <- mkMovieListItem
  Hooks.component "MovieList" \movies -> Hooks.do
    pure
      ( DOM.ul
          { className: "movie-list"
          , children: movies <#> movieListItem
          }
      )

mkMovieListItem :: Component Movie
mkMovieListItem =
  Hooks.component "MovieListItem" \movie -> Hooks.do
    isLoading /\ setIsLoading <- Hooks.useState' true
    pure
      ( DOM.li_
          [ DOM.div_ []
          , DOM.figure
              { className:
                  Monoid.guard isLoading "hidden"
              , children:
                  [ DOM.img
                      { src: movie.thumbnailURL
                      , onLoad: Events.handler_ do
                          setIsLoading false
                      }
                  ]
              }
          -- , DOM.h3_ [ DOM.text movie.title ]
          -- , DOM.span_ [ DOM.text ("(" <> show movie.year <> ")") ]
          -- , DOM.span_ [ DOM.text movie.director ]
          ]
      )