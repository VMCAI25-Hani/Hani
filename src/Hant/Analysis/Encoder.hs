{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Hant.Analysis.Encoder
  ( moduleSeparation,
    durationVarStr,
    nodeVarStr,
    syncTimeVarStr,
    syncValueVarStr,
    durationVarName,
    nodeVarName,
    variableVarName,
    syncTimeVarName,
    syncValueVarName,
  )
where

import Hant.Analysis.Trace (Index)
import Hant.Ast.Diagram (Name, Variable)
import Text.Printf (printf)

moduleSeparation :: String
moduleSeparation = "|"

durationVarStr :: String
durationVarStr = "$duration"

nodeVarStr :: String
nodeVarStr = "$node"

syncTimeVarStr :: String
syncTimeVarStr = "$syncTime"

syncValueVarStr :: String
syncValueVarStr = "$syncValue"

durationVarName :: Name -> Index -> String
durationVarName n i = printf "%s%s%s%s%d" durationVarStr moduleSeparation n moduleSeparation i

nodeVarName :: Name -> Index -> String
nodeVarName n i = printf "%s%s%s%s%d" nodeVarStr moduleSeparation n moduleSeparation i

variableVarName :: Name -> Variable -> Index -> String
variableVarName n v i = printf "%s%s%s%s%d" n moduleSeparation v moduleSeparation i

syncTimeVarName :: Name -> Index -> String
syncTimeVarName msgName i = printf "%s%s%s%s%d" syncTimeVarStr moduleSeparation msgName moduleSeparation i

syncValueVarName :: Name -> Index -> Index -> String
syncValueVarName msgName i i' = printf "%s%s%s%s%d%s%d" syncValueVarStr moduleSeparation msgName moduleSeparation i moduleSeparation i'