{-# LANGUAGE TemplateHaskell #-}

module Hant.Uxf.Uxf
  ( UMLType (..),
    DiagramType (..),
    RawDiagram (..),
    Basic (..),
    Element (..),
    Relation (..),
    elementType,
    x,
    y,
    w,
    h,
    content,
    element,
    sourceX,
    sourceY,
    targetX,
    targetY,
    parseUxfFile,
    parseUxfFolder,
    (=?),
  )
where

import Control.Applicative.Combinators (many)
import Control.Lens (makeLenses, to, (^.), (^..))
import Control.Lens.Combinators (folded)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (pack, unpack)
import Data.Foldable (find)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import Hant.Uxf.HtmlProcessor (processHtmlEntries)
import System.Directory (listDirectory)
import System.FilePath (takeFileName, (</>))
import Text.Megaparsec (MonadParsec (eof), Parsec, (<|>))
import Text.Megaparsec qualified as Mega
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, string)
import Text.Read (readMaybe)
import Xeno.DOM (Content (Text), Node, children, contents, name, parse)

data UMLType
  = UMLSequenceAllInOne
  | UMLNote
  | UMLSpecialState
  | UMLState
  | RelationType
  deriving (Eq, Show)

parseUMLType :: ByteString -> UMLType
parseUMLType s = case s of
  "UMLSequenceAllInOne" -> UMLSequenceAllInOne
  "UMLNote" -> UMLNote
  "UMLSpecialState" -> UMLSpecialState
  "UMLState" -> UMLState
  "Relation" -> RelationType
  _ -> UMLNote

data DiagramType
  = SD
  | HA
  deriving (Eq, Show)

parseDiagramType :: String -> DiagramType
parseDiagramType s
  | s `elem` sds = SD
  | s `elem` has = HA
  | otherwise = error ("wrong diagram type: " ++ s)
  where
    sds = [a ++ b | a <- ["s", "S"], b <- ["d", "D"]]
    has = [a ++ b | a <- ["h", "H"], b <- ["a", "A"]]

data Element = BasicE Basic | RelationE Relation
  deriving (Eq, Show)

data Basic = Basic
  { _elementType :: UMLType,
    _x :: Double,
    _y :: Double,
    _w :: Double,
    _h :: Double,
    _content :: Text
  }
  deriving (Eq, Show)

data Relation = Relation
  { _element :: Basic,
    _sourceX :: Double,
    _sourceY :: Double,
    _targetX :: Double,
    _targetY :: Double
  }
  deriving (Eq, Show)

data RawDiagram
  = RawDiagram Text DiagramType [Element]
  deriving (Eq, Show)

makeLenses ''Basic
makeLenses ''Relation
makeLenses ''Content

(=?) :: Element -> UMLType -> Bool
(BasicE b) =? t = (b ^. elementType) == t
(RelationE r) =? t = (r ^. element . elementType) == t

infixl 8 =?

parseUxf :: ByteString -> [Element]
parseUxf bs = case parse bs of
  Left _ -> []
  Right node -> nodeToElement <$> findChildrenByName "element" node

nodeToElement :: Node -> Element
nodeToElement n =
  let b = nodeToBasic n
   in if (b ^. elementType) /= RelationType
        then BasicE b
        else
          let aa =
                maybe
                  (error "no additional attributes")
                  (unpack . textContent)
                  (findChildByName "additional_attributes" n)
              al = splitOn ";" aa
              len = length al
              parseR i = fromMaybe (error "parsing relation failed") . readMaybe $ al !! i
              sx :: Double = parseR 0
              sy :: Double = parseR 1
              tx :: Double = parseR (len - 2)
              ty :: Double = parseR (len - 1)
           in RelationE $ Relation b sx sy tx ty

nodeToBasic :: Node -> Basic
nodeToBasic n =
  Basic et xi yi wi hi pa
  where
    et = parseUMLType $ maybe "" textContent (findChildByName "id" n)
    coord = fromMaybe (error "no coordinates") (findChildByName "coordinates" n)
    parseFromCoord s =
      fromMaybe (error ("parsing " ++ s ++ " failed"))
        . readMaybe
        . unpack
        . textContent
        . fromMaybe (error ("no " ++ s))
        $ findChildByName (pack s) coord
    xi :: Double = parseFromCoord "x"
    yi :: Double = parseFromCoord "y"
    wi :: Double = parseFromCoord "w"
    hi :: Double = parseFromCoord "h"
    pa = maybe (error "no attributes") (processHtmlEntries . decodeUtf8 . textContent) (findChildByName "panel_attributes" n)

findChildrenByName :: ByteString -> Node -> [Node]
findChildrenByName n parent = filter (\node -> name node == n) (children parent)

findChildByName :: ByteString -> Node -> Maybe Node
findChildByName n parent = find (\node -> name node == n) (children parent)

textContent :: Node -> ByteString
textContent node =
  mconcat (contents node ^.. folded . to text)
  where
    text c = case c of
      Text t -> t
      _ -> ""

type Parser a = Parsec Void String a

parseFilename :: String -> Maybe (String, DiagramType)
parseFilename s =
  let fnParser :: Parser (String, DiagramType)
      fnParser = do
        n <- many (alphaNumChar <|> char '-')
        void (string "_")
        t <- parseDiagramType <$> many letterChar
        void (string ".uxf")
        eof
        return (n, t)
   in Mega.parseMaybe fnParser s

parseUxfFile :: FilePath -> IO (Maybe RawDiagram)
parseUxfFile p =
  let fname = takeFileName p
      meta = parseFilename fname
   in do
        c <- BS.readFile p
        return ((\(n, t) -> RawDiagram (T.pack n) t (parseUxf c)) <$> meta)

parseUxfFolder :: FilePath -> IO [RawDiagram]
parseUxfFolder p = do
  files <- listDirectory p
  let absFiles = (p </>) <$> files
  catMaybes <$> traverse parseUxfFile absFiles