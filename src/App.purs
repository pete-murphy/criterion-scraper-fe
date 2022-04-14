module App
  ( mkApp
  ) where

import Prelude

import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Components.MUI.Autocomplete as Autocomplete
import Components.MUI.Slider (Range)
import Components.MUI.Slider as Slider
import Control.Alternative as Alternativ
import Control.Apply as Apply
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Control.Monad.Except as ExceptT
import Data.Argonaut as Argonaut
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..))
import Data.Enum (class Enum)
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Lens (Lens, Lens')
import Data.Lens as Lens
import Data.Lens.Fold ((^?))
import Data.Lens.Fold as Fold
import Data.Lens.Prism.Either (_Right)
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record as Lens.Record
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Monoid as Monoid
import Data.Ord.Max (Max(..))
import Data.Ord.Min (Min(..))
import Data.Semigroup.Foldable as Semigroup.Foldable
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic as Generic
import Data.String (Pattern(..))
import Data.String as String
import Data.Symbol (class IsSymbol)
import Data.Traversable as Traversable
import Debug as Debug
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Model.DateTime (DateTime)
import Prim.Row (class Cons)
import React.Basic as Basic
import React.Basic.DOM as DOM
import React.Basic.DOM.Events as DOM.Events
import React.Basic.Events as Events
import React.Basic.Hooks (Component, (/\))
import React.Basic.Hooks as Hooks
import React.Basic.Hooks.Aff as Hooks.Aff
import Record as Record
import Type.Proxy (Proxy(..))

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
      ( DOM.div_
          [ DOM.input
              { value: search
              , onChange: Events.handler DOM.Events.targetValue do
                  Foldable.traverse_ setSearch
              }
          , selectedYearsSlider case fetchMoviesResult of
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
          , autocomplete
              { options: options.directors
              , value: selectedDirectors
              , onChange: setSelectedDirectors
              , label: "Filter by director"
              }
          , autocomplete
              { options: options.countries
              , value: selectedCountries
              , onChange: setSelectedCountries
              , label: "Filter by country"
              }
          , sortMethodSelector
              { options: selectedSortMethods
              , setOptions: setSelectedSortMethods
              }

          , case fetchMoviesResult of
              Nothing -> mempty
              Just result' -> case result' of
                Left appError -> DOM.text (show appError)
                Right movies -> do
                  let moviesFiltered = filterMovies { search, selectedDirectors, selectedYears, selectedCountries } movies
                  movieList moviesFiltered
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
filterMovies { search, selectedYears, selectedDirectors, selectedCountries } =
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

mkSortMethodSelector
  :: Component
       { options :: Array SortMethod
       , setOptions :: (Array SortMethod -> Array SortMethod) -> Effect Unit
       }
mkSortMethodSelector = do
  Hooks.component "SortMethodSelector" \props -> Hooks.do
    isDragging /\ setIsDragging <- Hooks.useState' Nothing
    isDraggedOver /\ setIsDraggedOver <- Hooks.useState' Nothing
    localOptions /\ setLocalOptions <- Hooks.useState props.options

    -- Debug.traceM do (Array.insertAt 2 { key: Title, orientation: Ascending }) props.options
    pure do
      Basic.fragment
        [ DOM.text (show isDragging)
        , DOM.text (show isDraggedOver)
        , ( DOM.ul
              { className: "drag-ul"
              , onDrop: Events.handler_ do
                  Debug.traceM "drop"
              , children: localOptions `flip Array.mapWithIndex` \i option -> do
                  let
                    isDraggingOverOther = Maybe.fromMaybe false ado
                      dragging <- isDragging
                      draggedOver <- isDraggedOver
                      in dragging.i /= draggedOver
                    thisIsTheOneDragging = Maybe.maybe false (\dragging -> dragging.i == i) isDragging
                    thisIsTheOneDraggedOVer = Foldable.elem i isDraggedOver
                    shouldHideThisItem = thisIsTheOneDragging && isDraggingOverOther
                  Debug.trace ("This is the one dragging (" <> show i <> "): " <> show thisIsTheOneDragging) \_ -> do
                    Basic.fragment do
                      -- ( Monoid.guard (i `Foldable.elem` isDraggedOver && not (i `Foldable.elem` isDragging || (i - 1) `Foldable.elem` isDragging))
                      --     [ DOM.li_ []
                      --     ]
                      -- ) <>

                      [ DOM.li
                          { onDragStart: Events.handler_ do
                              Debug.traceM "dragStart"
                              -- Debug.traceM e
                              setIsDragging (Just { i, option })

                          , onDragOver: Events.handler_ do
                              -- Debug.traceM ("dragOver" <> show i)
                              setIsDraggedOver (Just i)
                          , onDragEnter: Events.handler_ do
                              Foldable.for_ isDragging \{ option } -> setLocalOptions \options ->
                                Maybe.fromMaybe options (Array.insertAt i option options)
                          , onDragLeave: Events.handler_ do
                              Foldable.for_ isDragging \{ option } -> setLocalOptions \_options -> props.options

                          , onDragEnd: Events.handler_ do
                              Debug.traceM "dragEnd"
                              setIsDragging Nothing
                              setIsDraggedOver Nothing
                          , children:
                              [ DOM.text (show option.key)
                              ]
                          , hidden: shouldHideThisItem
                          , draggable: true
                          }
                      ] <> (Monoid.guard (isDraggingOverOther && thisIsTheOneDraggedOVer) [ DOM.li_ [] ])
              }
          )
        ]

