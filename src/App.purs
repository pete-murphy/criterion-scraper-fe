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
import Control.Monad.ST as ST
import Data.Argonaut as Argonaut
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.ST as Array.ST
import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.Lens.Fold ((^?))
import Data.Lens.Prism.Either (_Right)
import Data.Lens.Prism.Maybe (_Just)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Monoid as Monoid
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Nullable as Nullable
import Data.Show.Generic as Generic
import Data.String (Pattern(..))
import Data.String as String
import Data.String.NonEmpty as String.NonEmpty
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Class as Effect
import Effect.Class.Console as Console
import Foreign.Hooks as Foreign.Hooks
import Foreign.Object as Object
import Model.DateTime (DateTime)
import Network.RemoteData (RemoteData(..), _Success)
import Network.RemoteData as RemoteData
import Prelude as Applicative
import React.Basic as Basic
import React.Basic.DOM as DOM
import React.Basic.DOM.Events as DOM.Events
import React.Basic.Events as Events
import React.Basic.Hooks (Component, (/\))
import React.Basic.Hooks as Hooks
import React.Basic.Hooks.Aff as Hooks.Aff
import React.Basic.Hooks.ResetToken as Hooks.ResetToken
import Web.URL.URLSearchParams as URLSearchParams

type Movie =
  { id :: String
  , title :: String
  , year :: Int
  , director :: String
  , country :: String
  , thumbnailURL :: String
  , detailsURL :: String
  , created :: DateTime
  , modified :: DateTime
  }

type Genre =
  { name :: String
  , displayName :: String
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

fetchMovies :: String -> ExceptT AppError Aff (Array Movie)
fetchMovies url = do
  response <- Except.withExceptT FetchError
    (ExceptT (Affjax.get ResponseFormat.json url))
  map amendMovieData <$> Except.withExceptT DecodeError
    (Except.except (Argonaut.decodeJson response.body))
  where
  amendMovieData movie =
    if movie.year == 0 && String.contains (Pattern "Symbiopsychotaxiplasm") movie.title -- Bad data
    then movie { year = 1968 }
    else movie

fetchGenres :: ExceptT AppError Aff (NonEmptyArray Genre)
fetchGenres = do
  let url = "http://localhost:8080/genre"
  response <- Except.withExceptT FetchError
    (ExceptT (Affjax.get ResponseFormat.json url))
  Except.withExceptT DecodeError
    (Except.except (Argonaut.decodeJson response.body))

fetchDirectors :: ExceptT AppError Aff (NonEmptyArray String)
fetchDirectors = do
  let url = "http://localhost:8080/director"
  response <- Except.withExceptT FetchError
    (ExceptT (Affjax.get ResponseFormat.json url))
  Except.withExceptT DecodeError
    (Except.except (Argonaut.decodeJson response.body))

fetchCountries :: ExceptT AppError Aff (NonEmptyArray String)
fetchCountries = do
  let url = "http://localhost:8080/country"
  response <- Except.withExceptT FetchError
    (ExceptT (Affjax.get ResponseFormat.json url))
  Except.withExceptT DecodeError
    (Except.except (Argonaut.decodeJson response.body))

mkURL
  :: { search :: String
     , selectedCountries :: Array String
     , selectedDirectors :: Array String
     , selectedGenres :: Array String
     , selectedSortBys :: Array (SortBy MovieField)
     , selectedYears :: Maybe { min :: Int, max :: Int }
     , limitPerPage :: Int
     }
  -> String
mkURL params = do
  let
    params' = Array.fromFoldable (String.NonEmpty.fromString params.search <#> \value -> { key: "search", value: String.NonEmpty.toString value })
      <> (params.selectedCountries <#> \value -> { key: "country", value })
      <> (params.selectedDirectors <#> \value -> { key: "director", value })
      <> (params.selectedGenres <#> \value -> { key: "genre", value })
      <> (params.selectedSortBys <#> sortByToQueryParamValue >>= Array.fromFoldable <#> \value -> { key: "sort-by", value })
      <>
        ( Foldable.fold
            ( params.selectedYears
                <#> \{ min, max } -> [ { key: "min-year", value: show min }, { key: "max-year", value: show max } ]
            )
        )
      <> [ { key: "limit", value: show params.limitPerPage } ]
    searchParams =
      URLSearchParams.fromString ""
        # unwrap (params' # Foldable.foldMap \{ key, value } -> Endo (URLSearchParams.append key value))
        # URLSearchParams.toString
  "http://localhost:8080/movie/paginated?" <> searchParams

sortByToQueryParamValue :: SortBy MovieField -> Maybe String
sortByToQueryParamValue (SortBy { order: Nothing }) = Nothing
sortByToQueryParamValue (SortBy { field, order: Just order }) = Just (field' <> ":" <> order')
  where
  field' = case field of
    Title -> "title"
    Year -> "year"
    Director -> "director"
    Country -> "country"
  order' = case order of
    Ascending -> "asc"
    Descending -> "desc"

-- TODO: Hard-coded based on full screen on my 13-inch laptop :)
limitPerPage :: Int
limitPerPage = 25

mkApp :: Component Unit
mkApp = do
  movieList <- mkMovieList
  selectedYearsSlider <- mkSelectedYearsSlider
  autocomplete <- Autocomplete.mkAutocomplete
  sortBysSelector <- mkSortBysSelector

  Hooks.component "App" \_ -> Hooks.do
    search /\ setSearch <- Hooks.useState' ""
    selectedYears /\ setSelectedYears <- Hooks.useState' Nothing
    selectedDirectors /\ setSelectedDirectors <- Hooks.useState' []
    selectedCountries /\ setSelectedCountries <- Hooks.useState' []
    selectedGenres /\ setSelectedGenres <- Hooks.useState' []
    selectedSortBys /\ setSelectedSortBys <- Hooks.useState
      [ SortBy { field: Title, order: Nothing }
      , SortBy { field: Year, order: Nothing }
      , SortBy { field: Director, order: Nothing }
      , SortBy { field: Country, order: Nothing }
      ]
    pageNumber /\ setPageNumber <- Hooks.useState 1
    hasNext /\ setHasNext <- Hooks.useState' true
    resetToken /\ reset <- Hooks.ResetToken.useResetToken

    -- Debounce the search & selected years inputs (these are the only inputs that can change rapidly)
    debouncedValues /\ setDebouncedValues <- Hooks.useState' { search, selectedYears }
    Hooks.Aff.useAff { search, selectedYears } do
      Aff.delay (Milliseconds 200.0)
      Effect.liftEffect (setDebouncedValues { search, selectedYears })

    let
      debouncedURL =
        mkURL
          { search: debouncedValues.search
          , selectedYears: debouncedValues.selectedYears
          , selectedDirectors
          , selectedCountries
          , selectedGenres
          , selectedSortBys
          -- , pageNumber
          -- Hard-coded above
          , limitPerPage
          }

    fetchMoviesResult /\ setFetchMoviesResult <- Hooks.useState (NotAsked :: RemoteData AppError (Array Movie))
    Hooks.Aff.useAff debouncedURL do
      Effect.liftEffect (Console.log debouncedURL)
      Effect.liftEffect (setPageNumber \_ -> 1)
      Effect.liftEffect do
        -- Transition to loading state if not yet fetched, otherwise no update
        setFetchMoviesResult case _ of
          NotAsked -> Loading
          x -> x
      result <- Except.runExceptT (fetchMovies debouncedURL)
      Effect.liftEffect (setFetchMoviesResult \_ -> RemoteData.fromEither result)
      -- Console.log "(((result ^? _Right) <#> Array.length) == Just limitPerPage)"
      -- Console.logShow (((result ^? _Right) <#> Array.length) == Just limitPerPage)
      Effect.liftEffect (setHasNext (((result ^? _Right) <#> Array.length) == Just limitPerPage))

    Hooks.Aff.useAff pageNumber do
      -- Do nothing if pageNumber is 1
      Applicative.when (pageNumber > 1 && hasNext) do
        let
          searchParams = URLSearchParams.fromString ""
            # URLSearchParams.append "offset" (show ((pageNumber - 1) * limitPerPage))
            # URLSearchParams.toString
        result <- Except.runExceptT (fetchMovies (debouncedURL <> "&" <> searchParams))
        Effect.liftEffect (setHasNext (((result ^? _Right) <#> Array.length) == Just limitPerPage))
        let
          resultRemoteData = RemoteData.fromEither result
          concatResults prevResult = case prevResult, resultRemoteData of
            Success prevMovies, Success nextMovies -> Success (prevMovies <> nextMovies)
            _, _ -> RemoteData.fromEither result

        Effect.liftEffect (setFetchMoviesResult concatResults)

    fetchGenresResult <- Hooks.Aff.useAff unit do
      Except.runExceptT fetchGenres
    fetchDirectorsResult <- Hooks.Aff.useAff unit do
      Except.runExceptT fetchDirectors
    fetchCountriesResult <- Hooks.Aff.useAff unit do
      Except.runExceptT fetchCountries

    genreOptions <- Hooks.useMemo (fetchGenresResult ^? _Just <<< _Right) \_ ->
      case fetchGenresResult of
        Just (Right genres) -> Array.fromFoldable genres
        _ -> mempty
    directorOptions <- Hooks.useMemo (fetchDirectorsResult ^? _Just <<< _Right) \_ ->
      case fetchDirectorsResult of
        Just (Right directors) -> Array.fromFoldable directors
        _ -> mempty
    countryOptions <- Hooks.useMemo (fetchCountriesResult ^? _Just <<< _Right) \_ ->
      case fetchCountriesResult of
        Just (Right countries) -> Array.fromFoldable countries
        _ -> mempty

    sentinelRef <- Hooks.useRef Nullable.null

    isIntersecting <- Foreign.Hooks.useIntersectionObserver sentinelRef
    Hooks.useEffect isIntersecting do
      Applicative.when (isIntersecting && hasNext) (setPageNumber (_ + 1))
      pure mempty

    -- Foreign.Hooks.useIntersectionObserver sentinelRef \isIntersecting -> do
    --   Console.logShow isIntersecting
    --   Applicative.when (isIntersecting && hasNext && RemoteData.isSuccess fetchMoviesResult) do
    --     setPageNumber (_ + 1)
    --   pure mempty

    pure
      ( DOM.main_
          [ DOM.section_
              [ DOM.div_
                  [ DOM.div
                      { className: "search"
                      , children:
                          [ DOM.label_ [ DOM.text "Search in title" ]
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
                          _ -> Interactive
                            -- TODO: Get from server
                            { bounds: { min: 1900, max: 2022 }
                            , thumbs: selectedYears
                            , setThumbs: setSelectedYears
                            }
                      -- _ -> NotInteractive
                      ]
                  , DOM.div_
                      [ autocomplete
                          { options: directorOptions
                          , value: selectedDirectors
                          , onChange: setSelectedDirectors
                          , label: "Filter by director"
                          }
                      ]
                  , DOM.div_
                      [ autocomplete
                          { options: countryOptions
                          , value: selectedCountries
                          , onChange: setSelectedCountries
                          , label: "Filter by country"
                          }
                      ]
                  , DOM.div_
                      [ autocomplete
                          { options: genreOptions <#> _.name
                          , value: selectedGenres
                          , onChange: setSelectedGenres
                          , label: "Filter by genre"
                          }
                      ]
                  , DOM.div_
                      [ DOM.label_ [ DOM.text "Sort by" ]
                      , sortBysSelector
                          { sortBys: selectedSortBys
                          , setSortBys: setSelectedSortBys
                          }
                      ]
                  ]
              ]
          , DOM.section_
              [ DOM.div_
                  [ case fetchMoviesResult of
                      NotAsked -> mempty
                      Loading -> mempty
                      Failure appError -> DOM.text (show (appError :: AppError))
                      Success movies ->
                        -- TODO: Handle empty array case
                        movieList (Array.fromFoldable movies)
                  ]
              , DOM.div { className: "sentinel", ref: sentinelRef }
              ]
          ]
      )

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
                  movie.id
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
          [ DOM.figure
              { _data:
                  Object.singleton "movie" movie.title

              , className:
                  Monoid.guard isLoading "hidden"
              , children:
                  [ DOM.div_
                      [ Hooks.element img
                          { src: movie.thumbnailURL <> "?auto=format%2Ccompress&q=100&w=380&crop=left&h=500&fit=crop"
                          , onLoad: Events.handler_ do
                              setIsLoading false
                          , loading: "lazy"
                          }
                      ]
                  ]
              }
          , DOM.div_
              [ DOM.h3_ [ DOM.text movie.title ]
              , DOM.div_ [ DOM.text (show movie.year) ]
              ]

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

data MovieField
  = Title
  | Year
  | Director
  | Country

derive instance Generic MovieField _
derive instance Eq MovieField
derive instance Ord MovieField

instance Show MovieField where
  show = Generic.genericShow

data SortOrder
  = Ascending
  | Descending

derive instance Eq SortOrder
derive instance Ord SortOrder

newtype SortBy a = SortBy { field :: a, order :: Maybe SortOrder }

rotateOrder :: Maybe SortOrder -> Maybe SortOrder
rotateOrder = case _ of
  Nothing -> Just Ascending
  Just Ascending -> Just Descending
  Just Descending -> Nothing

-- TODO: Not used
moveFromTo :: forall a. Int -> Int -> Array a -> Array a
moveFromTo sourceIndex targetIndex xs = ST.run do
  xs' <- Array.ST.thaw xs
  x <- Array.ST.splice sourceIndex 1 [] xs'
  _ <- Array.ST.splice targetIndex 0 x xs'
  Array.ST.freeze xs'

mkSortBysSelector
  :: Component
       { sortBys :: Array (SortBy MovieField)
       , setSortBys :: (Array (SortBy MovieField) -> Array (SortBy MovieField)) -> Effect Unit
       }
mkSortBysSelector = do
  dragAndDropList <- DragAndDropList.mkDragAndDropList
  Hooks.component "SortMethodSelector" \props -> Hooks.do
    pure
      ( dragAndDropList
          { items: props.sortBys
          , setItems: \sortBys -> props.setSortBys (\_ -> sortBys)
          , keyForItem: \(SortBy { field }) -> show field
          , isItemSelected: \(SortBy { order }) -> order /= Nothing
          , renderItem: \(SortBy { order, field }) -> DOM.div
              { className: "item-contents" <> Monoid.guard (order == Nothing) " neutral"
              , children:
                  ( case order of
                      Nothing -> []
                      Just Ascending -> [ DOM.div_ [ DOM.text "↑" ] ]
                      Just Descending -> [ DOM.div_ [ DOM.text "↓" ] ]
                  ) <>
                    [ DOM.div_ [ DOM.text (show field) ] ]
              }
          , onClickItem: \(SortBy clicked) -> props.setSortBys
              ( map \sortBy@(SortBy { field }) ->
                  if clicked.field == field then SortBy (clicked { order = rotateOrder clicked.order })
                  else sortBy
              )
          }
      )

