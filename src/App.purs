module App
  ( mkApp
  , moveFromTo
  ) where

import Prelude

import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Components.DragAndDropList as DragAndDropList
import Components.MUI.Autocomplete as Autocomplete
import Components.MUI.Slider (Range)
import Components.MUI.Slider as Slider
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Control.Monad.Except as ExceptT
import Control.Monad.ST as ST
import Data.Argonaut as Argonaut
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Array.ST as Array.ST
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.Lens.Fold ((^?))
import Data.Lens.Prism.Either (_Right)
import Data.Lens.Prism.Maybe (_Just)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Monoid as Monoid
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Semigroup.Foldable as Semigroup.Foldable
import Data.Set as Set
import Data.Show.Generic as Generic
import Data.String (Pattern(..))
import Data.String as String
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
  selectedYearsSlider <- mkSelectedYearsSlider
  autocomplete <- Autocomplete.mkAutocomplete
  sortMethodSelector <- mkSortMethodSelector

  Hooks.component "App" \_ -> Hooks.do
    fetchMoviesResult <- Hooks.Aff.useAff unit do
      Except.runExceptT fetchMovies
    search /\ setSearch <- Hooks.useState' ""
    selectedYears /\ setSelectedYears <- Hooks.useState' Nothing
    selectedDirectors /\ setSelectedDirectors <- Hooks.useState' []
    selectedCountries /\ setSelectedCountries <- Hooks.useState' []
    selectedSortMethods /\ setSelectedSortMethods <- Hooks.useState
      [ { key: Title, orientation: Neutral }
      , { key: Year, orientation: Neutral }
      , { key: Director, orientation: Neutral }
      , { key: Country, orientation: Neutral }
      ]
    let comparison = sortMethodsToComparison selectedSortMethods

    options <- Hooks.useMemo (fetchMoviesResult ^? _Just <<< _Right) \_ ->
      case fetchMoviesResult of
        Just (Right movies) ->
          case
            Semigroup.Foldable.foldMap1
              (\{ director, country } -> { directors: Set.singleton director, countries: Set.singleton country })
              movies
            of
            { directors, countries } -> { directors: Array.fromFoldable directors, countries: Array.fromFoldable countries }
        _ -> mempty

    pure
      ( DOM.main_
          [ DOM.section_
              [ DOM.div_
                  [ DOM.div
                      { className: "search"
                      , children:
                          [ DOM.label_ [ DOM.text "Search" ]
                          , DOM.input
                              { value: search
                              , type: "search"
                              , onChange: Events.handler DOM.Events.targetValue do
                                  Foldable.traverse_ setSearch
                              }
                          ]
                      }
                  , DOM.div_
                      [ selectedYearsSlider case fetchMoviesResult of
                          Just (Right movies) -> Interactive
                            { bounds:
                                case
                                  Semigroup.Foldable.foldMap1
                                    (\{ year } -> { min: Min year, max: Max year })
                                    movies
                                  of
                                  { min: Min min, max: Max max } -> { min, max }
                            , thumbs: selectedYears
                            , setThumbs: setSelectedYears
                            }
                          _ -> NotInteractive
                      ]
                  , DOM.div_
                      [ autocomplete
                          { options: options.directors
                          , value: selectedDirectors
                          , onChange: setSelectedDirectors
                          , label: "Filter by director"
                          }
                      ]
                  , DOM.div_
                      [ autocomplete
                          { options: options.countries
                          , value: selectedCountries
                          , onChange: setSelectedCountries
                          , label: "Filter by country"
                          }
                      ]
                  , DOM.div_
                      [ DOM.label_ [ DOM.text "Sort by" ]
                      , sortMethodSelector
                          { options: selectedSortMethods
                          , setOptions: setSelectedSortMethods
                          }
                      ]
                  ]
              ]
          , DOM.section_
              [ DOM.div_
                  [ case fetchMoviesResult of
                      Nothing -> mempty
                      Just result' -> case result' of
                        Left appError -> DOM.text (show appError)
                        Right movies -> do
                          let moviesFiltered = filterMovies { search, selectedDirectors, selectedYears, selectedCountries } movies
                          movieList (Array.sortBy comparison moviesFiltered)
                  ]
              ]
          ]
      )

filterMovies
  :: { search :: String
     , selectedYears :: Maybe Range
     , selectedDirectors :: Array String
     , selectedCountries :: Array String
     }
  -> NonEmptyArray Movie
  -> Array Movie
filterMovies { search, selectedYears, selectedDirectors, selectedCountries } = -- Array.take 10 <<<

  NonEmpty.filter \{ title, year, director, country } ->
    isInSelectedYears year
      && fuzzySearchAny [ title, director, country ]
      &&
        ( isSelectedDirector director
            || isSelectedCountry country
            || (Foldable.all Foldable.null [ selectedDirectors, selectedCountries ])
        )
  where
  isInSelectedYears year = Maybe.maybe true (\{ min, max } -> between min max year) selectedYears
  fuzzySearchAny = Foldable.any (String.contains (Pattern (String.toLower search)) <<< String.toLower)
  isSelectedDirector = flip Array.elem selectedDirectors
  isSelectedCountry = flip Array.elem selectedCountries

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

data SelectedYearsSliderProps
  = NotInteractive
  | Interactive
      { bounds :: Range
      , thumbs :: Maybe Range
      , setThumbs :: Maybe Range -> Effect Unit
      }

mkSelectedYearsSlider :: Component SelectedYearsSliderProps
mkSelectedYearsSlider = do
  slider <- Slider.mkSlider
  Hooks.component "selectedYearsSlider" \props -> Hooks.do
    enabled /\ setEnabled <- Hooks.useState false
    pure
      ( DOM.div_
          [ DOM.label
              { className: "check"
              , children:
                  [ DOM.input
                      { type: "checkbox"
                      , checked: enabled
                      , onChange: Events.handler_ do
                          setEnabled not
                      }
                  , DOM.text "Enable year filter"
                  ]
              }
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

data MovieKey
  = Title
  | Year
  | Director
  | Country

derive instance Generic MovieKey _
derive instance Eq MovieKey
derive instance Ord MovieKey

instance Show MovieKey where
  show = Generic.genericShow

data KeyOrientation
  = Neutral
  | Ascending
  | Descending

derive instance Generic KeyOrientation _
derive instance Eq KeyOrientation
derive instance Ord KeyOrientation

instance Show KeyOrientation where
  show = Generic.genericShow

type SortMethod = { key :: MovieKey, orientation :: KeyOrientation }

movieKeyToComparison :: MovieKey -> Movie -> Movie -> Ordering
movieKeyToComparison = case _ of
  Title -> comparing _.title
  Year -> comparing _.year
  Director -> comparing _.director
  Country -> comparing _.country

sortMethodsToComparison :: forall t. Foldable t => t SortMethod -> Movie -> Movie -> Ordering
sortMethodsToComparison = Foldable.foldMap interpret
  where
  interpret { orientation: Neutral } = mempty
  interpret { key, orientation: Ascending } = movieKeyToComparison key
  interpret { key, orientation: Descending } = flip (movieKeyToComparison key)

toggleOrientation :: KeyOrientation -> KeyOrientation
toggleOrientation = case _ of
  Neutral -> Ascending
  Ascending -> Descending
  Descending -> Neutral

-- TODO: Not used
moveFromTo :: forall a. Int -> Int -> Array a -> Array a
moveFromTo sourceIndex targetIndex xs = ST.run do
  xs' <- Array.ST.thaw xs
  x <- Array.ST.splice sourceIndex 1 [] xs'
  _ <- Array.ST.splice targetIndex 0 x xs'
  Array.ST.freeze xs'

mkSortMethodSelector
  :: Component
       { options :: Array SortMethod
       , setOptions :: (Array SortMethod -> Array SortMethod) -> Effect Unit
       }
mkSortMethodSelector = do
  dragAndDropList <- DragAndDropList.mkDragAndDropList
  Hooks.component "SortMethodSelector" \props -> Hooks.do
    pure
      ( dragAndDropList
          { items: props.options
          , setItems: \options -> props.setOptions (\_ -> options)
          , keyForItem: \option -> show option.key
          , isItemSelected: \option -> option.orientation /= Neutral
          , renderItem: \option -> DOM.div
              { className: "item-contents" <> Monoid.guard (option.orientation == Neutral) " neutral"
              , children:
                  ( case option.orientation of
                      -- Neutral -> [ DOM.div_ [ DOM.text "•" ] ]
                      Neutral -> []
                      Ascending -> [ DOM.div_ [ DOM.text "↑" ] ]
                      Descending -> [ DOM.div_ [ DOM.text "↓" ] ]
                  ) <>
                    [ DOM.div_ [ DOM.text (show option.key) ] ]
              }
          , onClickItem: \option -> props.setOptions
              ( map \option' ->
                  if option.key == option'.key then option { orientation = toggleOrientation option.orientation }
                  else option'
              )
          }
      )

