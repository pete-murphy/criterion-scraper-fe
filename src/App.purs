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
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Monoid as Monoid
import Data.String (Pattern(..))
import Data.String as String
import Effect.Aff (Aff)
import Foreign.Object as Object
import Model.DateTime (DateTime)
import React.Basic as Basic
import React.Basic.DOM as DOM
import React.Basic.DOM.Events as DOM.Events
import React.Basic.Events as Events
import React.Basic.Hooks (Component, (/\))
import React.Basic.Hooks as Hooks
import React.Basic.Hooks.Aff as Hooks.Aff

type Movie =
  { title :: String
  , year :: Int
  , director :: String
  , country :: String
  , movieId :: String
  , thumbnailURL :: String
  , detailsURL :: String
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
    search /\ setSearch <- Hooks.useState' ""

    minYear /\ setMinYear <- Hooks.useState' ""
    maxYear /\ setMaxYear <- Hooks.useState' ""

    pure
      ( DOM.div_
          [ DOM.input
              { value: search
              , onChange: Events.handler DOM.Events.targetValue do
                  Foldable.traverse_ setSearch
              }
          , DOM.input
              { value: minYear
              , onChange: Events.handler DOM.Events.targetValue do
                  Foldable.traverse_ setMinYear
              }
          , DOM.input
              { value: maxYear
              , onChange: Events.handler DOM.Events.targetValue do
                  Foldable.traverse_ setMaxYear
              }
          , case result of
              Nothing -> mempty
              Just result' -> case result' of
                Left appError -> DOM.text (show appError)
                Right movies -> do
                  let moviesFiltered = filterMovies search minYear maxYear movies
                  movieList moviesFiltered
          ]
      )

filterMovies :: String -> String -> String -> Array Movie -> Array Movie
filterMovies search minYear maxYear =
  Array.filter \{ title, year, director, country } ->
    (isInYearRange minYear maxYear year) &&
      ( String.contains (Pattern search) (String.toLower title)
          || String.contains (Pattern search) (String.toLower (show year))
          || String.contains (Pattern search) (String.toLower director)
          || String.contains (Pattern search) (String.toLower country)
      )
  where
  isInYearRange minYear' maxYear' year = Maybe.maybe true (\{ min, max } -> min <= year && year <= max)
    ado
      min <- Int.fromString minYear'
      max <- Int.fromString maxYear'
      in { min, max }

mkMovieList :: Component (Array Movie)
mkMovieList = do
  movieListItem <- mkMovieListItem
  Hooks.component "MovieList" \movies -> Hooks.do
    pure
      ( DOM.ul
          { className: "movie-list"
          , children: movies <#>
              \movie ->
                Basic.keyed
                  (movie.title <> movie.director)
                  (movieListItem movie)
          }
      )

mkMovieListItem :: Component Movie
mkMovieListItem = do
  -- TODO: Using this until `loading` attribute is added to `img`
  -- https://github.com/lumihq/purescript-react-basic-dom/issues/25
  img <- DOM.unsafeCreateDOMComponent "img"
  Hooks.component "MovieListItem" \movie -> Hooks.do
    isLoading /\ setIsLoading <- Hooks.useState' true
    pure
      ( DOM.li_
          [ DOM.div_ []
          , DOM.figure
              { _data:
                  Object.singleton "movie" movie.title

              -- , className:
              --     Monoid.guard isLoading "hidden"
              , children:
                  [ DOM.div_ [ DOM.text movie.title ]
                  , DOM.div_ [ DOM.text (show movie.year) ]
                  , DOM.div_ [ DOM.text movie.director ]
                  ]
              -- [ Hooks.element img
              --     { src: movie.thumbnailURL
              --     , onLoad: Events.handler_ do
              --         setIsLoading false
              --     , loading: "lazy"
              --     }
              -- ]
              }
          ]
      )