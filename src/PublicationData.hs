{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module PublicationData (Author (..), Publication (..)) where

import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Data.UUID (UUID)
import GHC.Generics

data Publication = Publication {
  uuid :: UUID,
  title :: String,
  authors :: [Author],
  url :: Maybe String
} deriving (Eq, Show, Generic)

data Author = Author {
  uuid :: UUID,
  name :: String
} deriving (Eq, Show, Generic)

instance NFData Publication where rnf = genericRnf
instance NFData Author where rnf = genericRnf
