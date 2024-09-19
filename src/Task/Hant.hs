module Task.Hant
  ( parallelExperiment1,
    parallelExperiment2,
    sequentialExperiment2,
    nopruningExperiment2,
    coverageExperiment1,
    coverageExperiment2,
  )
where

import Hant.Analysis.ParallelVerification (parallelAnalyzeLiteratureCase, analyzeSynthesizedCase, Mode (..))
import Hant.Pretty (banner)
import Hant.Synthesis.Synthesizer (SynthesisConfig (..), synthesizeCases)
import Hant.Util (LiteratureCase (..))
import System.FilePath ((</>))
import Hant.Analysis.Coverage (coverageLiteratureCase, coverageSynthesizedCase)

basePath :: FilePath
basePath = "./cases/Shan"

defaultBound :: Int
defaultBound = 3

synthesisConfig :: SynthesisConfig
synthesisConfig =
  SynthesisConfig
    { _caseNum = 10,
      _initialSeed = 2023,
      _checkingBound = defaultBound,
      _componentRange = (5, 10), -- key parameter, baseline: (5,10)
      _nodeRange = (4, 10), -- key parameter, baseline: (4,10)
      _edgeRange = (10, 20), -- key parameter, baseline: (10,20)
      _initialEdgeRange = (1, 4),
      _variableCountRange = (4, 8), -- key parameter, baseline: (4,10)
      _variableCountWithinNodeRange = (1, 3),
      _variableCountWithinAssignmentRange = (1, 3),
      _propertyCountRange = (0, 2),
      _constantRange = (0.0, 10.0),
      _itemCountRange = (1, 3),
      _loopBoundRange = (1, 3),
      _intCountRange = (1, 2),
      _intBoundRange = (1, 2),
      _priorityRange = (1, 10),
      _maxLayer = 3
    }

constructCase :: String -> Int -> LiteratureCase
constructCase n b =
  if b < 0
    then error "invalid bound"
    else
      LiteratureCase
        { name = n,
          path = basePath </> n,
          bound = b
        }

yield :: String -> LiteratureCase
yield = flip constructCase defaultBound

adcBugDInt :: String
adcBugDInt = "ADC-Bug-d-int"

adcBugInt :: String
adcBugInt = "ADC-Bug-int"

altitudeDisplay :: String
altitudeDisplay = "altitude-display"

altitudeDisplayInt :: String
altitudeDisplayInt = "altitude-display-int"

carController :: String
carController = "car-controller"

csmaAut :: String
csmaAut = "csma-aut"

fischerAut :: String
fischerAut = "fischer-aut"

hddi :: String
hddi = "hddi"

learningFactory :: String
learningFactory = "learning-factory"

medicalMonitor :: String
medicalMonitor = "medical-monitor"

waterTanks :: String
waterTanks = "water-tanks"

literatureCaseNames :: [String]
literatureCaseNames =
  [ adcBugDInt,
    adcBugInt,
    altitudeDisplay,
    altitudeDisplayInt,
    carController,
    csmaAut,
    fischerAut,
    hddi,
    learningFactory,
    medicalMonitor,
    waterTanks
  ]

banner1 :: IO ()
banner1 = banner "|  experiment 1: literature cases  |"

banner2 :: IO ()
banner2 = banner "|  experiment 2: synthesized cases  |"

parallelExperiment1 :: IO ()
parallelExperiment1 = do
  banner1
  mapM_ parallelAnalyzeLiteratureCase (yield <$> literatureCaseNames)

parallelExperiment2 :: IO ()
parallelExperiment2 = do
  banner2
  mapM_ (\c -> analyzeSynthesizedCase c Parallel) (synthesizeCases synthesisConfig)

sequentialExperiment2 :: IO ()
sequentialExperiment2 = do
  banner2
  mapM_ (\c -> analyzeSynthesizedCase c Sequential) (synthesizeCases synthesisConfig)

nopruningExperiment2 :: IO ()
nopruningExperiment2 = do
  banner2
  mapM_ (\c -> analyzeSynthesizedCase c NoPruning) (synthesizeCases synthesisConfig)

coverageExperiment1 :: IO ()
coverageExperiment1 = do
  mapM_ coverageLiteratureCase (yield <$> literatureCaseNames)

coverageExperiment2 :: IO ()
coverageExperiment2 = do
  mapM_ coverageSynthesizedCase (synthesizeCases synthesisConfig)
