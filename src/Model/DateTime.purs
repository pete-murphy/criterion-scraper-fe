module Model.DateTime where

import Prelude

import Data.Argonaut (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut as Argonaut
import Data.Either (Either)
import Data.Either as Either
import Data.PreciseDateTime as PreciseDateTime
import Data.RFC3339String (RFC3339String(..))

newtype DateTime = DateTime PreciseDateTime.PreciseDateTime

instance DecodeJson DateTime where
  decodeJson json = fromString =<< Argonaut.decodeJson json
    where
    fromString :: String -> Either JsonDecodeError DateTime
    fromString =
      map DateTime
        <<< Either.note (TypeMismatch "RFC3339String")
        <<< PreciseDateTime.fromRFC3339String
        <<< RFC3339String

derive newtype instance Show DateTime
derive newtype instance Eq DateTime

