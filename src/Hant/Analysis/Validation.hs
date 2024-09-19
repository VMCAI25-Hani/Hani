module Hant.Analysis.Validation
  ( validateDiagrams,
  )
where

import Data.Set ((\\))
import Data.Set qualified as S
import Hant.Ast.Diagram (Automaton, Diagrams, SequenceDiagram (SequenceDiagram), judgementVars, judgements, messages, mname, aname, iname)
import Text.Printf (printf)

validateDiagrams :: Diagrams -> [String]
validateDiagrams (sds, automata) = validateSds sds ++ checkConsistency sds automata

validateSds :: [SequenceDiagram] -> [String]
validateSds = concatMap validateSd

validateSd :: SequenceDiagram -> [String]
validateSd sd = messageNameNoCollision sd ++ checkMessageNamesInConstraints sd

-- all the method names declared in the sequence diagram should not collide
messageNameNoCollision :: SequenceDiagram -> [String]
messageNameNoCollision sd@(SequenceDiagram name _ _ _) =
  let nameInSd = mname <$> messages sd
      nameSet = S.fromList nameInSd
   in [printf "There is name collision in sequence diagram \"%s\"" name | length nameInSd /= S.size nameSet]

-- all the message names in the constraints are declared in the sequence diagram
checkMessageNamesInConstraints :: SequenceDiagram -> [String]
checkMessageNamesInConstraints sd@(SequenceDiagram name _ _ _) =
  let nameSet = S.fromList (mname <$> messages sd)
      nameInJudges = S.unions (judgementVars <$> judgements sd)
   in if nameInJudges `S.isSubsetOf` nameSet
        then []
        else
          let undeclared = nameInJudges \\ nameSet
           in [printf "In sequence diagram \"%s\", the following name in constraints are not declared: %s." name (show undeclared)]

checkConsistency :: [SequenceDiagram] -> [Automaton] -> [String]
checkConsistency sds han = concatMap (`checkConsistency'` han) sds

checkConsistency' :: SequenceDiagram -> [Automaton] -> [String]
checkConsistency' (SequenceDiagram n ins _ _) han = 
  let hanNames = S.fromList (aname <$> han)
      insNames = S.fromList (iname <$> ins)
   in if insNames `S.isSubsetOf` hanNames
        then []
        else
          let undeclared = insNames \\ hanNames
           in [printf "In sequence diagram \"%s\", the following instance names are not declared: %s." n (show undeclared)]