module SmartUnfit.Exercises
       ( Exercise(..)
       , ExerciseTechnique(..)
       , ExerciseCompletion
       , ExerciseDetails(..)
       , EquipmentId
       , EquipmentAdjustment(..)
       , MuscleGroup(..)
       , RepetitionStyle(..)
       , Series
       , TrainingSession
       , TimeInSeconds(..)
       , Weight(..)
       , defaultExercises
       , extractWeights
       ) where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Except (throwError)
import Data.DateTime (DateTime)
import Data.Generic.Rep as G
import Data.Generic.Rep.Show as GShow
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign (Foreign)
import Foreign as Foreign
import Simple.JSON as JSON

type EquipmentId = Int

data MuscleGroup =
    Dorsal
  | Biceps
  | Triceps
  | Pectoral
  | LowerLimbs
  | Shoulders
  | AbdomenLumbar
  | WholeBody

derive instance eqMuscleGroup :: Eq MuscleGroup
derive instance ordMuscleGroup :: Ord MuscleGroup

derive instance genericMuscleGroup :: G.Generic MuscleGroup _

instance showMuscleGroup :: Show MuscleGroup where
   show = GShow.genericShow

data EquipmentAdjustment =
    NumericAdjustment { adjustment :: Int, description :: String}
  | DescriptiveAdjustment { adjustment :: String, description :: String}

derive instance eqEquipmentAdjustment :: Eq EquipmentAdjustment
derive instance genericEquipmentAdjustment :: G.Generic EquipmentAdjustment _

instance showEquipmentAdjustment :: Show EquipmentAdjustment where
  show = GShow.genericShow

newtype TimeInSeconds = TimeInSeconds Int
data Weight =
    Weight Number
  | BodyWeight

derive instance genericWeight :: G.Generic Weight _
derive instance eqWeight :: Eq Weight

data RepetitionStyle =
     Repetitions { weight :: Weight, count :: Int }
   | UnilateralRepetitions { weight :: Weight, count :: Int }
   | RepetitionRange { weight :: Weight, minimum :: Int, maximum :: Int}
   | MaxRepetitions { weight :: Weight }
   | HoldPosture { weight :: Weight, howLong :: TimeInSeconds}

data ExerciseTechnique =
    RepetitionSequence (Array RepetitionStyle)
  | StayAtIt
  | TimeHoldingPosture TimeInSeconds

derive instance eqExerciseTechnique :: Eq ExerciseTechnique
derive instance eqRepetitionStyle :: Eq RepetitionStyle
derive instance eqTimeInSeconds :: Eq TimeInSeconds
derive instance newtypeTimeInSeconds :: Newtype TimeInSeconds _

derive instance genericExerciseTechnique :: G.Generic ExerciseTechnique _
derive instance genericRepetitionStyle :: G.Generic RepetitionStyle _

instance showTimeInseconds :: Show TimeInSeconds where
  show (TimeInSeconds t) = show t <> "\""

instance showExerciseTechnique :: Show ExerciseTechnique where
  show = GShow.genericShow

instance showRepetitionStyle :: Show RepetitionStyle where
  show v = GShow.genericShow v

data ExerciseDetails =
  WeightTrainingExercise
    { equipmentId :: Maybe EquipmentId
    , equipmentAdjustments :: Array EquipmentAdjustment
    , seriesCount :: Int
    }
  | BodyWeightExercise { place :: String }
  | AerobicExercise { timeInMinutes :: Int }

type Exercise =
  { description :: String
  , muscleGroup :: MuscleGroup
  , technique :: ExerciseTechnique
  , notes :: Maybe String
  , details :: ExerciseDetails
  }

type Series = Array Exercise

data ExerciseCompletion =
  ExerciseCompletion Exercise String

derive instance genericExerciseDetails :: G.Generic ExerciseDetails _
derive instance genericExerciseCompletion :: G.Generic ExerciseCompletion _

instance showExerciseDetails :: Show ExerciseDetails where
  show = GShow.genericShow

instance showExerciseCompletion :: Show ExerciseCompletion where
  show = GShow.genericShow

instance showWeight :: Show Weight where
  show (Weight w) = show w <> "kg"
  show BodyWeight = ""

type TrainingSession =
  { series :: Series
  , startedAt :: DateTime
  , endedAt :: Maybe DateTime
  , exerciseCompletionLog :: Array ExerciseCompletion
  }

-- JSON

class EnumReadForeign rep where
  enumReadForeignImpl :: Foreign -> Foreign.F rep

instance sumEnumReadForeign ::
  ( EnumReadForeign a
  , EnumReadForeign b
  ) => EnumReadForeign (G.Sum a b) where
  enumReadForeignImpl f
      = G.Inl <$> enumReadForeignImpl f
    <|> G.Inr <$> enumReadForeignImpl f

instance constructorArgsEnumReadForeign ::
  ( IsSymbol name
  , EnumReadForeign a
  ) => EnumReadForeign (G.Constructor name a) where
  enumReadForeignImpl f = do
    record :: { tag :: String, value :: Foreign } <- JSON.readImpl f

    if record.tag == name
      then G.Constructor <$> enumReadForeignImpl record.value
      else throwError <<< pure <<< Foreign.ForeignError $
        "Enum string " <> record.tag <> " did not match expected string " <> name
    where
      name = reflectSymbol (SProxy :: SProxy name)

instance argumentEnumReadForeign ::
  ( JSON.ReadForeign a
  ) => EnumReadForeign (G.Argument a) where
  enumReadForeignImpl f = G.Argument <$> JSON.readImpl f

instance noArgumentEnumReadForeign :: EnumReadForeign G.NoArguments where
  enumReadForeignImpl f = pure G.NoArguments

instance writeForeignRepetitionStyle ::
  JSON.WriteForeign RepetitionStyle where
  writeImpl (Repetitions value)
    = JSON.writeImpl { tag: "Repetitions"
                     , value
                     }

  writeImpl (UnilateralRepetitions value)
    = JSON.writeImpl { tag: "UnilateralRepetitions"
                     , value
                     }

  writeImpl (RepetitionRange value)
    = JSON.writeImpl { tag: "RepetitionRange"
                     , value
                     }

  writeImpl (MaxRepetitions value)
    = JSON.writeImpl { tag: "MaxRepetitions"
                     , value
                     }

  writeImpl (HoldPosture value)
    = JSON.writeImpl { tag: "HoldPosture"
                     , value
                     }

instance writeTimeInSeconds :: JSON.WriteForeign TimeInSeconds where
  writeImpl = JSON.writeImpl <<< unwrap

instance readTimeInSeconds :: JSON.ReadForeign TimeInSeconds where
  readImpl f = TimeInSeconds <$> Foreign.readInt f

instance writeForeignExerciseDetails :: JSON.WriteForeign ExerciseDetails where
  writeImpl (WeightTrainingExercise value)
    = JSON.writeImpl { tag: "WeightTrainingExercise", value }
  writeImpl (BodyWeightExercise value)
    = JSON.writeImpl { tag: "BodyWeightExercise", value }
  writeImpl (AerobicExercise value)
    = JSON.writeImpl { tag: "AerobicExercise", value }

instance readForeignEquipmentAdjustment :: JSON.ReadForeign EquipmentAdjustment where
  readImpl = enumReadForeign

instance readForeignExerciseDetails :: JSON.ReadForeign ExerciseDetails where
  readImpl = enumReadForeign

instance readRepetitionStyle :: JSON.ReadForeign RepetitionStyle where
  readImpl = enumReadForeign

instance readExerciseTechnique :: JSON.ReadForeign ExerciseTechnique where
  readImpl = enumReadForeign

instance writeForeignExerciseTechnique
         :: JSON.WriteForeign ExerciseTechnique where

  writeImpl (RepetitionSequence value)
    = JSON.writeImpl { tag: "RepetitionSequence"
                     , value
                     }

  writeImpl StayAtIt
    = JSON.writeImpl { tag: "StayAtIt" }

  writeImpl (TimeHoldingPosture value)
    = JSON.writeImpl { tag: "TimeHoldingPosture"
                     , value
                     }

instance writeForeignWeight :: JSON.WriteForeign Weight where
  writeImpl (Weight weight)
    = JSON.writeImpl { tag: "Weight"
                     , value: weight
                     }

  writeImpl BodyWeight
    = JSON.writeImpl { tag: "BodyWeight" }

instance readForeignWeight :: JSON.ReadForeign Weight where
  readImpl = enumReadForeign

instance writeForeignEquipmentAdjustment ::
  JSON.WriteForeign EquipmentAdjustment where

  writeImpl (NumericAdjustment value)
    = JSON.writeImpl { tag: "NumericAdjustment"
                     , value
                     }

  writeImpl (DescriptiveAdjustment value)
    = JSON.writeImpl { tag: "DescriptiveAdjustment"
                     , value
                     }

instance writeForeignMuscleGroup :: JSON.WriteForeign MuscleGroup where
  writeImpl mg = JSON.writeImpl { tag: show mg }

instance readForeignMuscleGroup :: JSON.ReadForeign MuscleGroup where
  readImpl = enumReadForeign

enumReadForeign
  :: forall a rep
   . G.Generic a rep
  => EnumReadForeign rep
  => Foreign
  -> Foreign.F a
enumReadForeign f = G.to <$> enumReadForeignImpl f

-- HELPERS

mkWeightTrainingExercise :: MuscleGroup
                         -> Maybe EquipmentId
                         -> String
                         -> Exercise
mkWeightTrainingExercise muscleGroup equipmentId description =
  let
    details =
      WeightTrainingExercise
      { equipmentId
      , equipmentAdjustments: []
      , seriesCount: 3
      }
  in
    { muscleGroup
    , description
    , details
    , notes: Nothing
    , technique: RepetitionSequence [ Repetitions { weight: Weight 5.0, count: 1 } ]
    }

defaultExercises :: Array Exercise
defaultExercises =
  [ mkWeightTrainingExercise Dorsal (Just 30) "Remada Máquina"
  , mkWeightTrainingExercise Dorsal (Just 34) "Puxada Polia Barra"
  , mkWeightTrainingExercise Dorsal (Just 34) "Puxada Polia Barra Frente"
  , mkWeightTrainingExercise Dorsal (Just 34) "Puxada Máquina"
  , mkWeightTrainingExercise Dorsal (Just 61) "Barra Fixa no Gravitron"
  , mkWeightTrainingExercise Dorsal (Just 62) "Serrátil"
  , mkWeightTrainingExercise Triceps (Just 45) "Tríceps Paralela"
  , mkWeightTrainingExercise Triceps (Just 52) "Tríceps Máquina"
  , mkWeightTrainingExercise Triceps (Just 62) "Tríceps Polia"
  , mkWeightTrainingExercise Triceps Nothing "Tríceps Testa"
  , mkWeightTrainingExercise Triceps Nothing "Tríceps Francês"
  , mkWeightTrainingExercise Biceps (Just 53) "Bíceps Máquina"
  , mkWeightTrainingExercise Biceps (Just 54) "Rosca Scott"
  , mkWeightTrainingExercise Biceps Nothing "Rosca Concentrada"
  , mkWeightTrainingExercise Biceps Nothing "Rosca Inversa"
  , mkWeightTrainingExercise Biceps Nothing "Rosca Direta"
  , mkWeightTrainingExercise Biceps Nothing "Rosca Alternada"
  , mkWeightTrainingExercise Pectoral (Just 20) "Supino Máquina"
  , mkWeightTrainingExercise Pectoral (Just 25) "Fly/Peck Deck"
  , mkWeightTrainingExercise Pectoral (Just 26) "Crucifixo Máquina"
  , mkWeightTrainingExercise Pectoral (Just 27) "Supino Livre Reto"
  , mkWeightTrainingExercise Pectoral (Just 28) "Supino Livre Inclinado"
  , mkWeightTrainingExercise Pectoral (Just 62) "Cross-Over"
  , mkWeightTrainingExercise Pectoral Nothing "Pullover"
  , mkWeightTrainingExercise Pectoral Nothing "Flexão de Braços"
  , mkWeightTrainingExercise LowerLimbs (Just 1) "Leg Press"
  , mkWeightTrainingExercise LowerLimbs (Just 2) "Cadeira Extensora"
  , mkWeightTrainingExercise LowerLimbs (Just 3) "Cadeira Flexora"
  , mkWeightTrainingExercise LowerLimbs (Just 4) "Cadeira Abdutora"
  , mkWeightTrainingExercise LowerLimbs (Just 5) "Cadeira Adutora"
  , mkWeightTrainingExercise LowerLimbs (Just 6) "Glúteos Máquina"
  , mkWeightTrainingExercise LowerLimbs (Just 7) "Gêmeos Sentado"
  , mkWeightTrainingExercise LowerLimbs (Just 8) "Gêmeos Máquina em Pé"
  , mkWeightTrainingExercise LowerLimbs (Just 9) "Leg Press 45"
  , mkWeightTrainingExercise LowerLimbs (Just 10) "Mesa Flexora"
  , mkWeightTrainingExercise LowerLimbs (Just 11) "Agachamento de Máquina"
  , mkWeightTrainingExercise LowerLimbs (Just 60) "Agachamento Smith"
  , mkWeightTrainingExercise LowerLimbs Nothing "Afundo"
  , mkWeightTrainingExercise LowerLimbs Nothing "Avanço"
  , mkWeightTrainingExercise LowerLimbs Nothing "Stiff"
  , mkWeightTrainingExercise Shoulders (Just 26) "Crucifixo Invertido Máquina"
  , mkWeightTrainingExercise Shoulders (Just 50) "Desenvolvimento Máquina"
  , mkWeightTrainingExercise Shoulders Nothing "Encolhimento"
  , mkWeightTrainingExercise Shoulders Nothing "Elevação Diagonal"
  , mkWeightTrainingExercise Shoulders Nothing "Elevação Lateral"
  , mkWeightTrainingExercise Shoulders Nothing "Elevação Frontal"
  , mkWeightTrainingExercise Shoulders Nothing "Remada Alta"
  , mkWeightTrainingExercise AbdomenLumbar (Just 40) "Abdominal Máquina"
  , mkWeightTrainingExercise AbdomenLumbar (Just 41) "Abdominal Hammer"
  , mkWeightTrainingExercise AbdomenLumbar (Just 43) "Lombar Máquina"
  , mkWeightTrainingExercise AbdomenLumbar (Just 44) "Hiperextensão Lombar"
  ]

extractWeights :: ExerciseTechnique -> Array Weight
extractWeights (RepetitionSequence seq) =
  map extractWeight seq
  where
    extractWeight (Repetitions { weight }) = weight
    extractWeight (UnilateralRepetitions { weight }) = weight
    extractWeight (RepetitionRange { weight }) = weight
    extractWeight (MaxRepetitions { weight }) = weight
    extractWeight (HoldPosture { weight }) = weight

extractWeights _ = []
