module Hant.Util
  ( LiteratureCase (..),
    Parser,
    symbolS,
    symbolW,
  )
where

import Control.Applicative.Combinators (many)
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Hant.Ast.Diagram (Bound)
import Text.Megaparsec (single)
import Text.Megaparsec qualified as Mega
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (symbol)

data LiteratureCase = LiteratureCase
  { name :: String,
    path :: FilePath,
    bound :: Bound
  }

type Parser a = Mega.Parsec Void Text a

-- match all tailing blank symbols
symbolS :: Text -> Parser Text
symbolS = symbol space

-- match tailing spaces
symbolW :: Text -> Parser Text
symbolW = symbol (void . many $ single ' ')