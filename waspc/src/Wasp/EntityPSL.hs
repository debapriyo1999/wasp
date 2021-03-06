-- TODO(matija): remove PSL suffix, added it to avoid clashes with the existing Entity module.
-- Once the old Entity module is removed, rename to "Entity".
module Wasp.EntityPSL
    ( EntityPSL (..)
    ) where

import Data.Text (Text)
import Data.Aeson (ToJSON(..), (.=), object)

data EntityPSL = EntityPSL
    { _name :: !String
    , _pslModelSchema :: !Text -- ^ PSL stands for Prisma Schema Language.
    } deriving (Show, Eq)

instance ToJSON EntityPSL where
    toJSON entity = object
        [ "name" .= _name entity
        , "pslModelSchema" .= _pslModelSchema entity
        ]
