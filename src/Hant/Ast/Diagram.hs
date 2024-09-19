module Hant.Ast.Diagram
  ( JudgeOp (..),
    Name,
    Scope,
    Priority,
    Bound,
    Variable,
    Expr (..),
    Dexpr (..),
    Judgement (..),
    Assignment (..),
    Differential (..),
    Instance (..),
    Event (..),
    Message (..),
    Item (..),
    Fragment (..),
    IntFragment (..),
    NodeType (..),
    Node (..),
    Edge (..),
    SequenceDiagram (..),
    Reachability (..),
    Property (..),
    Automaton (..),
    Diagrams,
    neg,
    sdname,
    mname,
    ename,
    aname,
    nname,
    iname,
    differentialVars,
    judgementVars,
    splitSequenceDiagram,
    messages,
    judgements,
    automatonVars,
    automatonInitialEdges,
    selectEdgeByName,
    nonInitialEdges,
    edgesToNodes,
    nodes,
    edges,
    edgeMap,
    nodeCount,
    edgeCount,
    nonInitEdges,
    namedHan,
    variables
  )
where

import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Text.Printf (printf)

data JudgeOp
  = Ge
  | Gt
  | Le
  | Lt
  | Eq
  | Neq
  deriving (Eq, Show)

type Name = Text

type Scope = Text

type Priority = Int

type Bound = Int

type Variable = Name

-- data Variable
--   = SimpleVariable Name
--   | ScopedVariable [Scope] Name
--   deriving (Eq, Show, Ord)

data Expr
  = Number Double
  | Var Variable
  | Negation Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Eq, Show)

data Dexpr
  = Dnumber Double
  | Nvar Variable
  | Dvar Variable
  | Dnegation Dexpr
  | Dadd Dexpr Dexpr
  | Dsub Dexpr Dexpr
  | Dmul Dexpr Dexpr
  | Ddiv Dexpr Dexpr
  deriving (Eq, Show)

data Judgement
  = SimpleJ Expr JudgeOp Expr
  | AndJ Judgement Judgement
  | OrJ Judgement Judgement
  deriving (Eq, Show)

data Assignment
  = Assignment Variable Expr
  deriving (Eq, Show)

data Differential
  = Differential Variable JudgeOp Dexpr
  deriving (Eq, Show)

data Instance
  = Instance Name Variable
  deriving (Eq, Show)

data Event
  = Event Name Instance
  deriving (Eq, Show)

data Message
  = Message Name Event Event [Assignment]
  deriving (Eq, Show)

data Item
  = ItemM Message
  | ItemF Fragment
  deriving (Eq, Show)

data Fragment
  = Block [Item]
  | AltF [Item] [Item]
  | IntF IntFragment
  | LoopF Bound Bound (Maybe Double) (Maybe Double) [Item]
  deriving (Eq, Show)

data IntFragment
  = IntFragment Priority Bound Bound [Item]
  deriving (Eq, Show)

data NodeType
  = Initial
  | Common
  deriving (Eq, Show)

data Node
  = Node NodeType Name (Set Variable) [Differential] [Judgement]
  deriving (Eq, Show)

data Edge
  = Edge Name Node Node (Maybe Judgement) [Assignment]
  deriving (Eq, Show)

data SequenceDiagram
  = SequenceDiagram Name [Instance] Fragment [Judgement]
  deriving (Eq, Show)

data Reachability
  = Reachable
  | Unreachable
  deriving (Eq, Show)

data Property
  = Property Name Reachability
  deriving (Eq, Show)

data Automaton
  = Automaton Name Node [Node] [Edge] [Property]
  deriving (Eq, Show)

type Diagrams = ([SequenceDiagram], [Automaton])

negateOp :: JudgeOp -> JudgeOp
negateOp Eq = Neq
negateOp Neq = Eq
negateOp Gt = Le
negateOp Ge = Lt
negateOp Lt = Ge
negateOp Le = Gt

neg :: Judgement -> Judgement
neg (SimpleJ e1 op e2) = SimpleJ e1 (negateOp op) e2
neg (AndJ j1 j2) = OrJ (neg j1) (neg j2)
neg (OrJ j1 j2) = AndJ (neg j1) (neg j2)

sdname :: SequenceDiagram -> Name
sdname (SequenceDiagram n _ _ _) = n

mname :: Message -> Name
mname (Message n _ _ _) = n

ename :: Edge -> Name
ename (Edge n _ _ _ _) = n

aname :: Automaton -> Name
aname (Automaton n _ _ _ _) = n

nname :: Node -> Name
nname (Node _ n _ _ _) = n

iname :: Instance -> Name
iname (Instance n _) = n

exprVars :: Expr -> Set Variable
exprVars (Number _) = S.empty
exprVars (Var v) = S.singleton v
exprVars (Negation e) = exprVars e
exprVars (Add e1 e2) = S.union (exprVars e1) (exprVars e2)
exprVars (Sub e1 e2) = S.union (exprVars e1) (exprVars e2)
exprVars (Mul e1 e2) = S.union (exprVars e1) (exprVars e2)
exprVars (Div e1 e2) = S.union (exprVars e1) (exprVars e2)

dexprVars :: Dexpr -> Set Variable
dexprVars (Dnumber _) = S.empty
dexprVars (Nvar v) = S.singleton v
dexprVars (Dvar v) = S.singleton v
dexprVars (Dnegation e) = dexprVars e
dexprVars (Dadd e1 e2) = S.union (dexprVars e1) (dexprVars e2)
dexprVars (Dsub e1 e2) = S.union (dexprVars e1) (dexprVars e2)
dexprVars (Dmul e1 e2) = S.union (dexprVars e1) (dexprVars e2)
dexprVars (Ddiv e1 e2) = S.union (dexprVars e1) (dexprVars e2)

differentialVars :: Differential -> Set Variable
differentialVars (Differential v _ e) = S.insert v (dexprVars e)

judgementVars :: Judgement -> Set Variable
judgementVars (SimpleJ e1 _ e2) = S.union (exprVars e1) (exprVars e2)
judgementVars (AndJ j1 j2) = S.union (judgementVars j1) (judgementVars j2)
judgementVars (OrJ j1 j2) = S.union (judgementVars j1) (judgementVars j2)

assignmentVars :: Assignment -> Set Variable
assignmentVars (Assignment v e) = S.insert v (exprVars e)

splitSequenceDiagram :: SequenceDiagram -> (Fragment, [IntFragment])
splitSequenceDiagram (SequenceDiagram _ _ frag _) = (fromMaybe (Block []) (clean frag), ints frag)

clean :: Fragment -> Maybe Fragment
clean (Block items) = Just $ Block (mapMaybe noInt items)
clean (AltF items1 items2) = Just $ AltF (mapMaybe noInt items1) (mapMaybe noInt items2)
clean (IntF {}) = Nothing
clean (LoopF l h s i items) = Just $ LoopF l h s i (mapMaybe noInt items)

noInt :: Item -> Maybe Item
noInt i@(ItemM _) = Just i
noInt (ItemF frag) = ItemF <$> clean frag

ints :: Fragment -> [IntFragment]
ints (Block items) = onlyInt items
ints (AltF items1 items2) = onlyInt items1 ++ onlyInt items2
ints (IntF i) = [i]
ints (LoopF _ _ _ _ items) = onlyInt items

onlyInt :: [Item] -> [IntFragment]
onlyInt = concatMap onlyInt'

onlyInt' :: Item -> [IntFragment]
onlyInt' (ItemM _) = []
onlyInt' (ItemF frag) = ints frag

messages :: SequenceDiagram -> [Message]
messages (SequenceDiagram _ _ frag _) =
  msgOfFrag frag
  where
    msgOfFrag :: Fragment -> [Message]
    msgOfFrag (Block items) = msgOfItems items
    msgOfFrag (AltF ifs elses) = msgOfItems (ifs ++ elses)
    msgOfFrag (IntF (IntFragment _ _ _ items)) = msgOfItems items
    msgOfFrag (LoopF _ _ _ _ items) = msgOfItems items
    msgOfItem :: Item -> [Message]
    msgOfItem (ItemM m) = [m]
    msgOfItem (ItemF f) = msgOfFrag f
    msgOfItems = concatMap msgOfItem

judgements :: SequenceDiagram -> [Judgement]
judgements (SequenceDiagram _ _ _ js) = js

automatonVars :: Automaton -> Set Variable
automatonVars (Automaton _ _ ns es _) =
  S.unions ((nodeVars <$> ns) ++ (edgeVars <$> es))
  where
    nodeVars (Node _ _ vars _ _) = vars
    edgeVars (Edge _ _ _ _ as) = S.unions (assignmentVars <$> as)

automatonInitialEdges :: Automaton -> [Edge]
automatonInitialEdges (Automaton _ _ _ es _) =
  filter isInitialEdge es
  where
    isInitial (Node Initial _ _ _ _) = True
    isInitial _ = False
    isInitialEdge (Edge _ s _ _ _) = isInitial s

selectEdgeByName :: Name -> Automaton -> Edge
selectEdgeByName n (Automaton _ _ _ es _) =
  let searchRes = filter (\(Edge n' _ _ _ _) -> n == n') es
   in case searchRes of
        [] -> error (printf "No edge with name %s" n)
        [e] -> e
        _ -> error (printf "Multiple edges with name %s" n)

nonInitialEdges :: [Edge] -> [Edge]
nonInitialEdges = filter (not . isInitialEdge)
  where
    isInitial (Node Initial _ _ _ _) = True
    isInitial _ = False
    isInitialEdge (Edge _ s _ _ _) = isInitial s

edgesToNodes :: [Edge] -> [Node]
edgesToNodes =
  nub . concatMap edgeToNodes
  where
    edgeToNodes (Edge _ n1 n2 _ _) = [n1, n2]

nodes :: Automaton -> [Node]
nodes (Automaton _ _ ns _ _) = ns

variables :: Automaton -> [Variable]
variables a = 
  S.toList $ foldl S.union S.empty $ variablesOfNode <$> nodes a
  where
    variablesOfNode (Node _ _ vs _ _) = vs

edges :: Automaton -> [Edge]
edges (Automaton _ _ _ es _) = es

edgeMap :: Automaton -> Map (Name, Name) Name
edgeMap (Automaton _ _ _ es _) =
  M.fromList [((nname s, nname t), n) | Edge n s t _ _ <- es]

nodeCount :: Automaton -> Int
nodeCount = length . nodes

edgeCount :: Automaton -> Int
edgeCount = length . edges

nonInitEdges :: Automaton -> [Edge]
nonInitEdges = filter (not . isInitialEdge) . edges
  where
    isInitial (Node Initial _ _ _ _) = True
    isInitial _ = False
    isInitialEdge (Edge _ s _ _ _) = isInitial s

namedHan :: [Automaton] -> Map Name Automaton
namedHan = M.fromList . map (\a -> (aname a, a))