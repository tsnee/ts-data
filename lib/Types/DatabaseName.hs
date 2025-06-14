{-# LANGUAGE DerivingStrategies #-}

module Types.DatabaseName (DatabaseName (..)) where

newtype DatabaseName = DatabaseName String
  deriving newtype (Eq, Ord, Show)
