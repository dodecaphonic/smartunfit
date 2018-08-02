module SmartUnfit.Regimen (Regimen, currentRegimen) where

import Prelude

import Data.Array (catMaybes, find)
import Data.Maybe (Maybe(..))
import SmartUnfit.Exercises (EquipmentAdjustment(..), Exercise, ExerciseDetails(..), ExerciseTechnique(..), MuscleGroup(..), RepetitionStyle(..), Series, TimeInSeconds(..), Weight(..), defaultExercises)

type Regimen = Array Series

singleRep :: Number -> Int -> ExerciseTechnique
singleRep w i = RepetitionSequence [ Repetitions (Weight w) i ]

bodyWeightSingleRep :: Int -> ExerciseTechnique
bodyWeightSingleRep i = RepetitionSequence [ Repetitions BodyWeight i ]

repRange :: Number -> Int -> Int -> ExerciseTechnique
repRange w a b = RepetitionSequence [ RepetitionRange (Weight w) a b ]

sequencedReps :: Number -> Int -> Int -> ExerciseTechnique
sequencedReps w a b = RepetitionSequence [ Repetitions (Weight w) a, Repetitions (Weight w) b ]

timedRep :: Number -> Int -> ExerciseTechnique
timedRep w t = RepetitionSequence [ HoldPosture (Weight w) (TimeInSeconds t) ]

adjustExercise :: String
               -> Array EquipmentAdjustment
               -> ExerciseTechnique
               -> Maybe String
               -> Maybe Exercise
adjustExercise name adj tec notes = adjust <$> exercise
  where
    exercise = find (\e -> e.description == name) defaultExercises
    adjust e =
      case e.details of
        (WeightTrainingExercise wte) ->
          e { notes = notes
            , technique = tec
            , details = WeightTrainingExercise
                          wte { equipmentAdjustments = adj }
            }
        _ -> e

seriesA :: Series
seriesA =
  catMaybes $
    [ Just { description: "Aquecimento (Transport)"
           , details: AerobicExercise { timeInMinutes: 15 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    , adjustExercise "Crucifixo Máquina"
                     [ NumericAdjustment 9 "Banco"
                     , NumericAdjustment 3 "Amplitude"
                     ]
                     ( RepetitionSequence
                         [ HoldPosture (Weight 22.5) (TimeInSeconds 10)
                         , UnilateralRepetitions (Weight 22.5) 8
                         , Repetitions (Weight 22.5) 10
                         ]
                     )
                     Nothing
    , adjustExercise "Tríceps Máquina"
                     [ NumericAdjustment 5 "Banco" ]
                     ( sequencedReps 27.5 15 15 )
                     Nothing
    , adjustExercise "Supino Livre Reto"
                     []
                     ( RepetitionSequence [ MaxRepetitions (Weight 7.5) ] )
                     ( Just "HBL" )
    , adjustExercise "Flexão de Braços"
                     []
                     ( RepetitionSequence [ MaxRepetitions BodyWeight ] )
                     Nothing
    , Just { description: "Tiro na Esteira"
           , details: AerobicExercise { timeInMinutes: 5 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    , adjustExercise "Leg Press 45"
                     []
                     ( RepetitionSequence
                         [ HoldPosture (Weight 30.0) (TimeInSeconds 10)
                         , Repetitions (Weight 30.0) 12
                         ]
                     )
                     Nothing
    , adjustExercise "Gêmeos Sentado"
                     []
                     ( singleRep 30.0 15 )
                     Nothing
    , adjustExercise "Cadeira Extensora"
                     [ NumericAdjustment 6 "Banco"
                     , NumericAdjustment 11 "Amplitude"
                     , DescriptiveAdjustment "L" "Pés"
                     ]
                     ( RepetitionSequence
                         [ HoldPosture (Weight 17.5) (TimeInSeconds 10)
                         , UnilateralRepetitions (Weight 17.5) 12
                         ]
                     )
                     Nothing
    , Just { description: "Abdominal Banco Declinado"
           , details: BodyWeightExercise { place: "Banco Declinado" }
           , muscleGroup: AbdomenLumbar
           , notes: Nothing
           , technique: StayAtIt
           }
    , Just { description: "Aeróbico Livre (Transport)"
           , details: AerobicExercise { timeInMinutes: 15 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    ]

seriesB :: Series
seriesB =
  catMaybes $
    [ Just { description: "Aquecimento (Bike)"
           , details: AerobicExercise { timeInMinutes: 15 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    , adjustExercise "Remada Máquina"
                     [ NumericAdjustment 6 "Assento"
                     , NumericAdjustment 7 "Peito" ]
                     (sequencedReps 27.5 8 10)
                     (Just "Unilateral Neutro + Bilateral Supinado")
    , adjustExercise "Rosca Scott"
                     []
                     (singleRep 10.0 10)
                     (Just "Direto + Inverso")
    , adjustExercise "Puxada Polia Barra"
                     [ NumericAdjustment 3 "Prendedor do Joelho" ]
                     (singleRep 35.5 10)
                     (Just "Barra romana")
    , Just { description: "Tiro no Transport"
           , details: AerobicExercise { timeInMinutes: 5 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    , adjustExercise "Barra Fixa no Gravitron"
                     []
                     (RepetitionSequence [ MaxRepetitions (Weight 54.0) ])
                     (Just "Pegada Aberta")
    , adjustExercise "Cadeira Flexora"
                     [ NumericAdjustment 5 "Encosto"
                     , NumericAdjustment 0 "Amplitude"
                     , DescriptiveAdjustment "XL" "Pés"
                     ]
                     ( RepetitionSequence
                         [ HoldPosture (Weight 27.5) (TimeInSeconds 10)
                         , Repetitions (Weight 27.5) 10
                         , HoldPosture (Weight 27.5) (TimeInSeconds 10)
                         ]
                     )
                     Nothing
    , adjustExercise "Cadeira Abdutora"
                     []
                     ( RepetitionSequence
                         [ MaxRepetitions (Weight 65.0)
                         , MaxRepetitions (Weight 72.0)
                         , MaxRepetitions (Weight 80.0) ]
                     )
                     Nothing
    , adjustExercise "Hiperextensão Lombar"
                     []
                     (bodyWeightSingleRep 15)
                     Nothing
    , Just { description: "Aeróbico Livre (Esteira HIIT)"
           , details: AerobicExercise { timeInMinutes: 10 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    ]

currentRegimen :: Regimen
currentRegimen = [seriesA, seriesB]
