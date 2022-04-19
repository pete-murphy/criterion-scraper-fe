module Control.Monad.ExceptT
  ( runExceptT'
  , runParExceptT'
  , ExceptT'
  , ParExceptT'
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except as Except
import Control.Parallel as Parallel
import Control.Parallel.Class (class Parallel)
import Data.Either as Either
import Data.Functor.Compose (Compose(..))
import Data.Newtype as Newtype
import Data.Validation.Semigroup (V)
import Data.Validation.Semigroup as V
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

newtype ExceptT' e m a = ExceptT' (Except.ExceptT e m a)

derive newtype instance Functor m => Functor (ExceptT' e m)
derive newtype instance Monad m => Apply (ExceptT' e m)
derive newtype instance Monad m => Applicative (ExceptT' e m)
derive newtype instance Monad m => Bind (ExceptT' e m)
derive newtype instance Monad m => Monad (ExceptT' e m)
derive newtype instance MonadEffect m => MonadEffect (ExceptT' e m)
derive newtype instance MonadAff m => MonadAff (ExceptT' e m)
derive newtype instance Monad m => MonadThrow e (ExceptT' e m)

runExceptT' :: forall e m. ExceptT' e m ~> Except.ExceptT e m
runExceptT' (ExceptT' x) = x

newtype ParExceptT' e m a = ParExceptT' (Compose m (V e) a)

derive newtype instance Functor m => Functor (ParExceptT' e m)
derive newtype instance (Semigroup e, Apply m) => Apply (ParExceptT' e m)
derive newtype instance (Semigroup e, Applicative m) => Applicative (ParExceptT' e m)

runParExceptT' :: forall e f. ParExceptT' e f ~> Compose f (V e)
runParExceptT' (ParExceptT' x) = x

instance (Semigroup e, Parallel f m) => Parallel (ParExceptT' e f) (ExceptT' e m) where
  parallel =
    ParExceptT'
      <<< Compose
      <<< map (Either.either V.invalid pure)
      <<< Parallel.parallel
      <<< Except.runExceptT
      <<< runExceptT'
  sequential =
    ExceptT'
      <<< Except.ExceptT
      <<< map (V.toEither)
      <<< Parallel.sequential
      <<< Newtype.unwrap
      <<< runParExceptT'