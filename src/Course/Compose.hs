{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import           Course.Applicative
import           Course.Apply
import           Course.Bind
import           Course.Core
import           Course.Functor

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
-- (a->b) -> Compose f g a -> Compose f g b
instance (Functor f,Functor g) => Functor (Compose f g) where
  (<$>) ab (Compose fg) = Compose ((ab <$>) <$> fg)

-- Implement the (<*>) function for an Apply instance for Compose
-- Compose f g (a->b) -> Compose f g a -> Compose f g b
instance (Apply f,Apply g) => Apply (Compose f g) where
  (<*>) (Compose f) (Compose g) = Compose ( (<*>) <$> f <*> g)

-- Implement the pure function for an Applicative instance for Compose
instance (Applicative f,Applicative g) => Applicative (Compose f g) where
  pure a = Compose (pure (pure a))

-- Implement the (=<<) function for a Bind instance for Compose
instance (Bind f,Bind g) => Bind (Compose f g) where
  (=<<) = error "impossible"
