{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns #-}

module Serve.ClubMetadata (processClubMetadataRequest) where

import TextShow.Data.Time ()
import Prelude

import MonadStack (AppM)
import Types.ClubMetadataResponse (ClubMetadataResponse (..))

processClubMetadataRequest :: Int -> AppM ClubMetadataResponse
processClubMetadataRequest = undefined
