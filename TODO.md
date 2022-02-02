## Features

- [x] Filters for year range

- [ ] Autocomplete search (multiple values) for each of: (Director, Country, (Tags?))

  - [ ] Wrap MUI Autocomplete component?

- [ ] Ability to sort (year-first hi-lo, lo-hi; title-first ...)

  - List ( SortMethod )
    - SortMethod :: Ord a => (a -> a -> Ordering)

- [ ] Lazy-loading / infinite-scrolling

- [ ] Fuzzy-search text?
- [ ] Suggestions if no matches?

## Bugs

- [ ] Director is nullable

## Probably no longer relevant

- [ ] Use blurhash for images?
  - Can use smaller images now ...

## Stretch goals

(These will entail BE changes)

- [ ] Ability to "tag" movies

  - [ ] Persist those tags (in localStorage / DB)

- [ ] Integrate with Letterboxd API (would need to learn the OAuth2 flow)

## Meta goals

- [ ] Implement Figma design spec
