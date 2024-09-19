module Hant.Uxf.HtmlProcessor
  ( processHtmlEntries,
  )
where

import Data.Text (Text, replace)

processHtmlEntries :: Text -> Text
processHtmlEntries =
  replace "&gt;" ">"
    . replace "&lt;" "<"
    . replace "&amp;" "&"
