{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Hant.Ast.Diagram.Parser
  ( parseDiagram,
    parseSequenceDiagram,
    parseAutomaton,
    parseJudgement,
    judgeOpParser,
    variableParser,
    nameParser,
  )
where

import Control.Applicative.Combinators (many)
import Control.Lens ((^.), _Just)
import Control.Monad (void)
import Control.Monad.Combinators ((<|>))
import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix), makeExprParser)
import Data.Functor.Identity (Identity)
import Data.List (find)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Hant.Ast.Diagram
  ( Assignment (Assignment),
    Automaton (Automaton),
    Bound,
    Dexpr (..),
    Differential (Differential),
    Edge (Edge),
    Event (..),
    Expr (..),
    Fragment (..),
    Instance (Instance),
    IntFragment (IntFragment),
    Item (ItemF, ItemM),
    JudgeOp (..),
    Judgement (AndJ, OrJ, SimpleJ),
    Message (Message),
    Name,
    Node (..),
    NodeType (..),
    Priority,
    Property (..),
    Reachability (..),
    SequenceDiagram (SequenceDiagram),
    Variable,
    differentialVars,
    judgementVars,
  )
import Hant.Util (Parser, symbolS, symbolW)
import Hant.Uxf.Uxf (Basic, DiagramType (..), Element (BasicE, RelationE), RawDiagram (..), Relation, UMLType (..), content, element, elementType, h, sourceX, sourceY, targetX, targetY, w, x, y, (=?))
import Text.Megaparsec (MonadParsec (eof, try), anySingle, between, choice, manyTill, optional, parse, single, (<?>))
import Text.Megaparsec qualified as Mega
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, newline, space)
import Text.Megaparsec.Char.Lexer (decimal, float, lexeme)

type Location = (Double, Double, Double, Double)

keywords :: [String]
keywords = ["valign"]

parseDiagram :: RawDiagram -> Either SequenceDiagram Automaton
parseDiagram d@(RawDiagram _ SD _) = Left $ parseSequenceDiagram d
parseDiagram d@(RawDiagram _ HA _) = Right $ parseAutomaton d

parseSequenceDiagram :: RawDiagram -> SequenceDiagram
parseSequenceDiagram (RawDiagram _ _ es) =
  let sd = findElementSD es ^. content
      note = findElementNote es ^. _Just . content
      (n, ins, f) = parseSDBody sd
      props = parseConstraints note
   in SequenceDiagram n ins f props

parseSDBody :: Text -> (Name, [Instance], Fragment)
parseSDBody s = case parse sdBodyParser "" s of
  Left e -> error ("parse sequence diagram failed: " ++ Mega.errorBundlePretty e)
  Right res -> res

parseConstraints :: Text -> [Judgement]
parseConstraints s = case parse judgementsParser "" s of
  Left e -> error ("parse constraints failed: " ++ Mega.errorBundlePretty e)
  Right res -> res

parseProperties :: Text -> [Property]
parseProperties note = case parse propertiesParser "" note of
  Left e -> error ("parse properties failed: " ++ Mega.errorBundlePretty e)
  Right properties -> properties

parseAutomaton :: RawDiagram -> Automaton
parseAutomaton (RawDiagram title _ es) =
  let i@(_, initialNode) = parseInitial $ findInitialNode es
      locNodes = parseNode <$> findNodes es
      nodes = snd <$> locNodes
      locList = i : locNodes
      relations = parseEdge locList <$> findRelations es
      note = findElementNote es ^. _Just . content
      props = parseProperties note
   in Automaton title initialNode nodes relations props

parseInitial :: Basic -> (Location, Node)
parseInitial b =
  let loc = parseLocation b
   in (loc, Node Initial "" S.empty [] [])

parseEdge :: [(Location, Node)] -> Relation -> Edge
parseEdge locs r =
  let source = searchSource locs r
      target = searchTarget locs r
      (n, j, as) = case parse edgeParser "" (r ^. element . content) of
        Left e -> error ("parse edge failed: " ++ Mega.errorBundlePretty e)
        Right res -> res
   in Edge n source target j as

edgeParser :: Parser (Name, Maybe Judgement, [Assignment])
edgeParser = do
  space
  void (symbolS "lt" *> symbolS "=" *> symbolS "->")
  n <- nameParser
  res <- optional tailParser
  let (j, as) = fromMaybe (Nothing, []) res
  return (n, j, as)
  where
    tailParser :: Parser (Maybe Judgement, [Assignment])
    tailParser = do
      space
      void (symbolW ":")
      j <- optional judgementParser
      void (symbolW ";")
      as <- assignmentsParser
      return (j, as)

searchSource :: [(Location, Node)] -> Relation -> Node
searchSource locs r =
  let sx = (r ^. element . x) + (r ^. sourceX)
      sy = (r ^. element . y) + (r ^. sourceY)
   in case searchNode locs (sx, sy) of
        Nothing -> error ("search source node failed: " ++ T.unpack (r ^. element . content))
        Just n -> n

searchTarget :: [(Location, Node)] -> Relation -> Node
searchTarget locs r =
  let tx = (r ^. element . x) + (r ^. targetX)
      ty = (r ^. element . y) + (r ^. targetY)
   in case searchNode locs (tx, ty) of
        -- Nothing -> error ("search target node failed: " ++ T.unpack (r ^. element . content))
        Nothing -> error ("search target node failed: " ++ show locs ++ show r)
        Just n -> n

searchNode :: [(Location, Node)] -> (Double, Double) -> Maybe Node
searchNode locs loc = snd <$> find (\(l, _) -> inSquare l loc) locs

inSquare :: Location -> (Double, Double) -> Bool
inSquare (x1, x2, y1, y2) (dotx, doty) =
  x1 <= dotx
    && x2 >= dotx
    && y1 <= doty
    && y2 >= doty

parseNode :: Basic -> (Location, Node)
parseNode b =
  let loc = parseLocation b
      node = parseNodeContent (b ^. content)
   in (loc, node)

parseNodeContent :: Text -> Node
parseNodeContent s =
  let (n, vset, diffs, judges) = case parse nodeContentParser "" s of
        Left e -> error ("parse node content failed: " ++ Mega.errorBundlePretty e)
        Right res -> res
   in Node Common n vset diffs judges

nodeContentParser :: Parser (Name, S.Set Variable, [Differential], [Judgement])
nodeContentParser = do
  space
  title <- manyTill anySingle newline
  void (symbolS "--")
  diffs <- many differentialParser
  space
  void (symbolS "-.")
  judges <- manyTill judgementParser (optional nodeContentEnd <* eof)
  let diffVars = S.unions (differentialVars <$> diffs)
  let judgeVars = S.unions (judgementVars <$> judges)
  return (T.pack title, S.union diffVars judgeVars, diffs, judges)
  where
    nodeContentEnd :: Parser Text
    nodeContentEnd = symbolS "valign" <* symbolS "=" <* symbolS "top"

parseLocation :: Basic -> Location
parseLocation b =
  let xv = b ^. x
      yv = b ^. y
      wv = b ^. w
      hv = b ^. h
   in (xv, xv + wv, yv, yv + hv)

differentialParser :: Parser Differential
differentialParser = do
  void (symbolS "'")
  v <- variableParser
  op <- judgeOpParser
  dexpr <- dexprParser
  void newline <|> eof
  return $ Differential v op dexpr

parseJudgement :: Text -> Judgement
parseJudgement j = case parse judgementParser "" j of
  Left e -> error ("parse judgement failed: " ++ Mega.errorBundlePretty e)
  Right judge -> judge

sdBodyParser :: Parser (Name, [Instance], Fragment)
sdBodyParser = do
  space
  title <- titleParser
  void (many newline)
  ins <- many instanceParser
  void (many newline)
  let insMap = M.fromList ((\i@(Instance _ v) -> (v, i)) <$> ins)
  items <- itemsParser insMap
  return (title, ins, Block items)

titleParser :: Parser Name
titleParser = do
  space
  void (symbolS "title")
  void (symbolS "=")
  n <- nameParser
  void (many newline)
  return n

instanceParser :: Parser Instance
instanceParser = do
  space
  void (symbolS "obj")
  void (symbolS "=")
  n <- nameParser
  void (symbolS "~")
  v <- nameParser
  void (many newline)
  return $ Instance n v

itemsParser :: M.Map Variable Instance -> Parser [Item]
itemsParser insMap = many (itemParser insMap)

itemParser :: M.Map Variable Instance -> Parser Item
itemParser insMap = (ItemF <$> fragmentParser insMap) <|> (ItemM <$> messageParser insMap)

fragmentParser :: M.Map Variable Instance -> Parser Fragment
fragmentParser insMap =
  choice
    [ try (loopFragmentParser insMap),
      try (altFragmentParser insMap),
      intFragmentParer insMap
    ]

loopFragmentParser :: M.Map Variable Instance -> Parser Fragment
loopFragmentParser insMap = do
  space
  void (symbolS "combinedFragment")
  void (symbolS "=")
  (lower, upper, maybeStart, maybeInterval) <- loopHeader
  void (symbolS "~")
  void (optional nameParser)
  void (many newline)
  items <- itemsParser insMap
  void (symbolS "--")
  void (many newline)
  return (LoopF lower upper maybeStart maybeInterval items)
  where
    loopHeader :: Parser (Bound, Bound, Maybe Double, Maybe Double)
    loopHeader = do
      space
      void (symbolS "loop")
      (lower, upper) <- boundParser
      assigns <- assignmentsParser
      let assignMap = M.fromList ((\(Assignment v e) -> (v, e)) <$> assigns)
      let start = M.lookup "start" assignMap >>= exprToNumber
      let interval = M.lookup "interval" assignMap >>= exprToNumber
      return (lower, upper, start, interval)
    exprToNumber (Number n) = Just n
    exprToNumber _ = Nothing

intFragmentParer :: M.Map Variable Instance -> Parser Fragment
intFragmentParer insMap = do
  space
  void (symbolS "combinedFragment")
  void (symbolS "=")
  (p, lower, upper) <- intHeader
  void (symbolS "~")
  void (optional nameParser)
  void (many newline)
  items <- itemsParser insMap
  void (symbolS "--")
  void (many newline)
  return $ IntF $ IntFragment p lower upper items
  where
    intHeader :: Parser (Priority, Bound, Bound)
    intHeader = do
      space
      void (symbolS "int")
      void (symbolS "(")
      void (symbolS "p")
      void (symbolS "=")
      p <- lexeme space decimal
      void (symbolS ")")
      maybeBound <- optional boundParser
      let (lower, upper) = fromMaybe (1, 1) maybeBound
      return (p, lower, upper)

altFragmentParser :: M.Map Variable Instance -> Parser Fragment
altFragmentParser insMap = do
  space
  void (symbolS "combinedFragment")
  void (symbolS "=")
  void (symbolS "alt")
  void (symbolS "~")
  void (optional nameParser)
  void (many newline)
  items <- itemsParser insMap
  void (symbolS "..")
  void (many newline)
  altItems <- itemsParser insMap
  void (symbolS "--")
  void (many newline)
  return $ AltF items altItems

boundParser :: Parser (Bound, Bound)
boundParser = do
  void (symbolS "(")
  lower <- lexeme space decimal
  void (symbolS ",")
  upper <- lexeme space decimal
  void (symbolS ")")
  return (lower, upper)

messageParser :: M.Map Variable Instance -> Parser Message
messageParser insMap = do
  space
  sv <- variableParser
  void (symbolS "->>>")
  tv <- variableParser
  void (symbolS ":")
  (n, sn, tn) <- nameTriple
  assigns <- assignmentsParser
  void (optional $ symbolS ";")
  void (many newline)
  let si = case M.lookup sv insMap of
        Just i -> i
        Nothing -> missing (show sv)
  let ti = case M.lookup tv insMap of
        Just i -> i
        Nothing -> missing (show sv)
  return $ Message n (Event sn si) (Event tn ti) assigns
  where
    missing s = error ("instance variable not declared: " ++ s)
    nameTriple :: Parser (Name, Name, Name)
    nameTriple = do
      void (symbolW "(")
      n <- nameParser
      void (symbolW ",")
      sn <- nameParser
      void (symbolW ",")
      tn <- nameParser
      void (symbolW ")")
      return (n, sn, tn)

propertiesParser :: Parser [Property]
propertiesParser = many propertyParser <?> "properties"

propertyParser :: Parser Property
propertyParser = do
  void (symbolW "(")
  n <- nameParser
  void (symbolW ",")
  r <- reachabilityParser
  void (symbolS ")")
  return $ Property n r

reachabilityParser :: Parser Reachability
reachabilityParser =
  choice
    [ Reachable <$ symbolS "reachable",
      Unreachable <$ symbolS "unreachable"
    ]
    <?> "reachability"

judgementsParser :: Parser [Judgement]
judgementsParser = many judgementParser <?> "judgements"

judgementParser :: Parser Judgement
judgementParser =
  (makeExprParser simple table <* space) <?> "judgement"
  where
    simple = do
      l <- exprParser
      op <- judgeOpParser
      r <- exprParser
      return $ SimpleJ l op r
    table =
      [ [binary "&&" AndJ],
        [binary "||" OrJ]
      ]

judgeOpParser :: Parser JudgeOp
judgeOpParser =
  choice
    [ Ge <$ symbolS ">=",
      Gt <$ symbolS ">",
      Le <$ symbolS "<=",
      Lt <$ symbolS "<",
      try (Eq <$ symbolS "=="),
      Eq <$ symbolS "=",
      Neq <$ symbolS "!="
    ]
    <?> "judgement operations"

assignmentsParser :: Parser [Assignment]
assignmentsParser = do
  space
  leftB <- optional (symbolS "[")
  case leftB of
    Nothing -> return []
    Just _ -> do
      fstAssign <- optional assignmentParser
      assigns <- case fstAssign of
        Nothing -> return []
        Just f -> do
          tails <- many tailParser
          return (f : tails)
      void (symbolS "]")
      return assigns
  where
    tailParser :: Parser Assignment
    tailParser = do
      void (symbolS ",")
      assignmentParser

assignmentParser :: Parser Assignment
assignmentParser = do
  space
  v <- variableParser
  void (symbolS ":=")
  expr <- exprParser
  return $ Assignment v expr

dexprParser :: Parser Dexpr
dexprParser =
  makeExprParser terms table <?> "dexpr"
  where
    terms =
      choice
        [ Dnumber <$> numberParser,
          Nvar <$> variableParser,
          Dvar <$> (symbolS "'" *> variableParser),
          parens dexprParser
        ]
    table =
      [ [ prefix "-" Dnegation,
          prefix "+" id
        ],
        [ binary "*" Dmul,
          binary "/" Ddiv
        ],
        [ binary "+" Dadd,
          binary "-" Dsub
        ]
      ]

exprParser :: Parser Expr
exprParser =
  makeExprParser terms table <?> "expr"
  where
    terms =
      choice
        [ Number <$> numberParser,
          Var <$> variableParser,
          parens exprParser
        ]
    table =
      [ [ prefix "-" Negation,
          prefix "+" id
        ],
        [ binary "*" Mul,
          binary "/" Div
        ],
        [ binary "+" Add,
          binary "-" Sub
        ]
      ]

binary ::
  Text ->
  (a -> a -> a) ->
  Operator (Mega.ParsecT Void Text Identity) a
binary op f = InfixL (f <$ symbolS op)

prefix ::
  Text ->
  (a -> a) ->
  Operator (Mega.ParsecT Void Text Identity) a
prefix op f = Prefix (f <$ symbolS op)

parens :: Parser a -> Parser a
parens = between (symbolW "(") (symbolW ")")

variableParser :: Parser Variable
variableParser =
  ( do
      space
      nameParser
  )
    <?> "variable"

nameParser :: Parser Text
nameParser =
  ( do
      firstChar <- letterChar
      restChars <- many (alphaNumChar <|> char '_')
      void (many (single ' '))
      let v = firstChar : restChars
      if v `elem` keywords
        then fail "keywords cannot be used as variables"
        else return $ T.pack v
  )
    <?> "name"

numberParser :: Parser Double
numberParser =
  lexeme (void (many (single ' '))) (try float <|> fmap intToDouble decimal) <?> "number"
  where
    intToDouble :: Integer -> Double
    intToDouble = fromIntegral

findRelations :: [Element] -> [Relation]
findRelations =
  filter isRelation . mapMaybe toRelation
  where
    toRelation (BasicE _) = Nothing
    toRelation (RelationE r) = Just r
    isRelation r = (r ^. element . elementType) == RelationType

findBasicUnsafe :: [Element] -> UMLType -> Basic
findBasicUnsafe es t =
  let fs = filter (=? t) es
   in if null fs
        then error ("no expected element: " ++ show t)
        else
          let e = head fs
           in case e of
                BasicE b -> b
                RelationE _ -> error "impossible"

findElementSD :: [Element] -> Basic
findElementSD es = findBasicUnsafe es UMLSequenceAllInOne

findElementNote :: [Element] -> Maybe Basic
findElementNote es =
  find (=? UMLNote) es >>= elementToBasic
  where
    elementToBasic (BasicE b) = Just b
    elementToBasic (RelationE _) = Nothing

findInitialNode :: [Element] -> Basic
findInitialNode es = findBasicUnsafe es UMLSpecialState

findNodes :: [Element] -> [Basic]
findNodes =
  mapMaybe toBasic
  where
    toBasic (BasicE b) =
      if (b ^. elementType) == UMLState
        then Just b
        else Nothing
    toBasic (RelationE _) = Nothing