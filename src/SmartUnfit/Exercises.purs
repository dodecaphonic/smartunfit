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

import Data.Maybe

import Data.Bounded (class Ord)
import Data.DateTime (DateTime)
import Data.Eq (class Eq)
import Data.Functor (map)
import Data.Generic.Rep as G
import Data.Generic.Rep.Show as GShow
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

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
    NumericAdjustment Int String
  | DescriptiveAdjustment String String

derive instance eqEquipmentAdjustment :: Eq EquipmentAdjustment
derive instance genericEquipmentAdjustment :: G.Generic EquipmentAdjustment _

instance showEquipmentAdjustment :: Show EquipmentAdjustment where
  show = GShow.genericShow

newtype TimeInSeconds = TimeInSeconds Int
data Weight =
    Weight Number
  | BodyWeight

derive instance eqWeight :: Eq Weight

data RepetitionStyle =
     Repetitions Weight Int
   | UnilateralRepetitions Weight Int
   | RepetitionRange Weight Int Int
   | MaxRepetitions Weight
   | HoldPosture Weight TimeInSeconds

data ExerciseTechnique =
    RepetitionSequence (Array RepetitionStyle)
  | StayAtIt
  | TimeHoldingPosture TimeInSeconds

derive instance eqExerciseTechnique :: Eq ExerciseTechnique
derive instance eqRepetitionStyle :: Eq RepetitionStyle
derive instance eqTimeInSeconds :: Eq TimeInSeconds

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
  | BodyWeightExercise
    { place :: String }
  | AerobicExercise
    { timeInMinutes :: Int }

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
    , technique: RepetitionSequence [ Repetitions (Weight 5.0) 1 ]
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
    extractWeight (Repetitions w _) = w
    extractWeight (UnilateralRepetitions w _) = w
    extractWeight (RepetitionRange w _ _) = w
    extractWeight (MaxRepetitions w) = w
    extractWeight (HoldPosture w _) = w

extractWeights _ = []
