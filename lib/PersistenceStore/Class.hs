module PersistenceStore.Class (None, Persistable, PersistenceType, Retrievable, retrieve, save) where

import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))

class PersistenceType a
data None
instance PersistenceType None

class (PersistenceType t) => Persistable t a m r where
  save :: Proxy t -> a -> m r
instance Persistable None a Identity a where
  save _ = Identity

class (PersistenceType t) => Retrievable t q m a where
  retrieve :: Proxy t -> q -> m (Maybe a)
instance Retrievable None a Identity a where
  retrieve _ q = Identity $ Just q
