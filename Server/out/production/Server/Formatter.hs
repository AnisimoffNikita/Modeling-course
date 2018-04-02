module Formatter where

import Text.PrettyPrint.Boxes
import Data.Aeson (ToJSON, encode)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (Text)
import Data.Text.Encoding

debugTableFormat :: (Show a) => [[a]] -> String
debugTableFormat cols = render $ hsep 5 left (map (vcat left . map text) stringed)
  where
    stringed = map (map show) cols

tableJsonLazy :: ToJSON a => a -> Text
tableJsonLazy  = encodeToLazyText

