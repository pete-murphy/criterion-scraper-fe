module App
  ( mkApp
  ) where

import Prelude

import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Components.MUI.Slider (Range)
import Components.MUI.Slider as Slider
import Control.Apply as Apply
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Control.Monad.Except as ExceptT
import Data.Argonaut as Argonaut
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Monoid as Monoid
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Semigroup.Foldable as Semigroup.Foldable
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable as Traversable
import Debug as Debug
import Effect (Effect)
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

fetchMovies :: ExceptT AppError Aff (NonEmptyArray Movie)
fetchMovies = do
  let url = "http://167.99.230.0:8080/movies"
  response <- ExceptT.withExceptT FetchError
    (ExceptT (Affjax.get ResponseFormat.json url))
  map amendMovieData <$> ExceptT.withExceptT DecodeError
    (Except.except (Argonaut.decodeJson response.body))
  where
  amendMovieData movie =
    if movie.year == 0 && String.contains (Pattern "Symbiopsychotaxiplasm") movie.title -- Bad data
    then movie { year = 1968 }
    else movie

mkApp :: Component Unit
mkApp = do
  movieList <- mkMovieList
  yearFilterSlider <- mkYearFilterSlider
  Hooks.component "App" \_ -> Hooks.do
    result <- Hooks.Aff.useAff unit do
      Except.runExceptT fetchMovies
    search /\ setSearch <- Hooks.useState' ""
    yearFilter /\ setYearFilter <- Hooks.useState' Nothing

    pure
      ( DOM.div_
          [ DOM.input
              { value: search
              , onChange: Events.handler DOM.Events.targetValue do
                  Foldable.traverse_ setSearch
              }
          , yearFilterSlider case result of
              Just (Right movies) -> Interactive
                { bounds:
                    case
                      Semigroup.Foldable.foldMap1
                        (\{ year } -> { min: Min year, max: Max year })
                        movies
                      of
                      { min: Min min, max: Max max } -> { min, max }
                , thumbs: yearFilter
                , setThumbs: setYearFilter
                }
              _ -> NotInteractive

          , case result of
              Nothing -> mempty
              Just result' -> case result' of
                Left appError -> DOM.text (show appError)
                Right movies -> do
                  let moviesFiltered = filterMovies search yearFilter movies
                  movieList moviesFiltered
          ]
      )

filterMovies :: String -> Maybe Range -> NonEmptyArray Movie -> Array Movie
filterMovies search yearFilter = Array.take 24 <<<
  NonEmpty.filter \{ title, year, director, country } ->
    isYearInFilter year
      &&
        ( stringContainsSearch title
            || stringContainsSearch director
            || stringContainsSearch country
        )
  where
  isYearInFilter year = Maybe.maybe true (\{ min, max } -> between min max year) yearFilter
  stringContainsSearch = String.contains (Pattern (String.toLower search)) <<< String.toLower

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
                  movie.movieId
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

              , className:
                  Monoid.guard isLoading "hidden"
              , children:
                  -- [ DOM.div_ [ DOM.text movie.title ]
                  -- , DOM.div_ [ DOM.text (show movie.year) ]
                  -- , DOM.div_ [ DOM.text movie.director ]
                  -- ]
                  [ Hooks.element img
                      { src: movie.thumbnailURL <> "?auto=format%2Ccompress&q=100&w=400"
                      , onLoad: Events.handler_ do
                          setIsLoading false
                      , loading: "lazy"
                      }
                  ]
              }
          ]
      )

data YearFilterSliderProps
  = NotInteractive
  | Interactive
      { bounds :: Range
      , thumbs :: Maybe Range
      , setThumbs :: Maybe Range -> Effect Unit
      }

mkYearFilterSlider :: Component YearFilterSliderProps
mkYearFilterSlider = do
  slider <- Slider.mkSlider
  Hooks.component "YearFilterSlider" \props -> Hooks.do
    enabled /\ setEnabled <- Hooks.useState false
    pure
      ( DOM.div_
          [ DOM.label_
              [ DOM.input
                  { type: "checkbox"
                  , checked: enabled
                  , onChange: Events.handler_ do
                      setEnabled not
                  }
              , DOM.text "Enable year filter"
              ]
          , case props of
              NotInteractive -> mempty
              Interactive { thumbs, setThumbs, bounds } -> do
                let
                  thumbs' = Maybe.fromMaybe bounds thumbs
                  setThumbs' = setThumbs <<< Just
                if enabled then
                  DOM.div_
                    [ slider
                        { minDistance: 10
                        , bounds
                        , thumbs: thumbs'
                        , setThumbs: setThumbs'
                        }
                    , DOM.text (show thumbs)
                    ]
                else mempty
          ]
      )