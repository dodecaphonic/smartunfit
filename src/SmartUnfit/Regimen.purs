module SmartUnfit.Regimen (Regimen, currentRegimen) where

import Prelude

import Data.Array (catMaybes, find)
import Data.Maybe (Maybe(..))
import SmartUnfit.Exercises (EquipmentAdjustment(..), Exercise, ExerciseDetails(..), ExerciseTechnique(..), MuscleGroup(..), RepetitionStyle(..), Series, TimeInSeconds(..), Weight(..), defaultExercises)

type Regimen = Array Series

singleRep :: Number -> Int -> ExerciseTechnique
singleRep weight count
  = RepetitionSequence [ Repetitions { weight: Weight weight, count } ]

bodyWeightSingleRep :: Int -> ExerciseTechnique
bodyWeightSingleRep count
  = RepetitionSequence [ Repetitions { weight: BodyWeight, count } ]

repRange :: Number -> Int -> Int -> ExerciseTechnique
repRange weight minimum maximum
  = RepetitionSequence
      [ RepetitionRange { weight: Weight weight, minimum, maximum } ]

sequencedReps :: Number -> Int -> Int -> ExerciseTechnique
sequencedReps weight a b
  = RepetitionSequence
      [ Repetitions { weight: Weight weight, count: a }
      , Repetitions { weight: Weight weight, count: b } ]

timedRep :: Number -> Int -> ExerciseTechnique
timedRep weight howLong
  = RepetitionSequence
      [ HoldPosture { weight: Weight weight, howLong: TimeInSeconds howLong }
      ]

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
                     [ NumericAdjustment { adjustment: 9, description: "Banco" }
                     , NumericAdjustment { adjustment: 3, description: "Amplitude" }
                     ]
                     ( RepetitionSequence
                         [ HoldPosture { weight: Weight 30.0, howLong: TimeInSeconds 10 }
                         , UnilateralRepetitions { weight: Weight 30.0, count: 8 }
                         , Repetitions { weight: Weight 30.0, count: 10 }
                         ]
                     )
                     Nothing
    , adjustExercise "Tríceps Máquina"
                     [ NumericAdjustment { adjustment: 5, description: "Banco" } ]
                     ( sequencedReps 32.5 15 15 )
                     Nothing
    , adjustExercise "Supino Livre Reto"
                     []
                     ( RepetitionSequence [ MaxRepetitions { weight: Weight 12.0 } ] )
                     ( Just "HBL" )
    , adjustExercise "Flexão de Braços"
                     []
                     ( RepetitionSequence [ MaxRepetitions { weight: BodyWeight } ] )
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
                         [ HoldPosture { weight: Weight 40.0, howLong: TimeInSeconds 10 }
                         , Repetitions { weight: Weight 40.0, count: 12 }
                         ]
                     )
                     Nothing
    , adjustExercise "Gêmeos Sentado"
                     []
                     ( singleRep 40.0 20 )
                     Nothing
    , adjustExercise "Cadeira Extensora"
                     [ NumericAdjustment { adjustment: 6, description: "Banco" }
                     , NumericAdjustment { adjustment: 11, description: "Amplitude" }
                     , DescriptiveAdjustment { adjustment: "L", description: "Pés"}
                     ]
                     ( RepetitionSequence
                         [ HoldPosture { weight: Weight 22.5, howLong: TimeInSeconds 10 }
                         , UnilateralRepetitions { weight: Weight 22.5, count: 12 }
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
                     [ NumericAdjustment { adjustment: 6, description: "Assento" }
                     , NumericAdjustment { adjustment: 7, description: "Peito" } ]
                     (sequencedReps 32.5 8 10)
                     (Just "Unilateral Neutro + Bilateral Supinado")
    , adjustExercise "Rosca Scott"
                     []
                     (singleRep 18.0 10)
                     (Just "Direto")
    , adjustExercise "Rosca Scott"
                     []
                     (singleRep 14.0 10)
                     (Just "Inverso")
    , adjustExercise "Puxada Polia Barra"
                     [ NumericAdjustment { adjustment: 3, description: "Prendedor do Joelho" } ]
                     (singleRep 40.0 10)
                     (Just "Barra romana")
    , Just { description: "Tiro no Transport"
           , details: AerobicExercise { timeInMinutes: 5 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    , adjustExercise "Barra Fixa no Gravitron"
                     []
                     (RepetitionSequence [ MaxRepetitions { weight: Weight 47.0 } ])
                     (Just "Pegada Aberta")
    , adjustExercise "Cadeira Flexora"
                     [ NumericAdjustment { adjustment: 5, description: "Encosto" }
                     , NumericAdjustment { adjustment: 0, description: "Amplitude" }
                     , DescriptiveAdjustment { adjustment: "XL", description: "Pés" }
                     ]
                     ( RepetitionSequence
                         [ HoldPosture { weight: Weight 32.5, howLong: TimeInSeconds 10 }
                         , Repetitions { weight: Weight 32.5, count: 10 }
                         , HoldPosture { weight: Weight 32.5, howLong: TimeInSeconds 10 }
                         ]
                     )
                     Nothing
    , adjustExercise "Cadeira Abdutora"
                     []
                     ( RepetitionSequence
                         [ MaxRepetitions { weight: Weight 65.0 }
                         , MaxRepetitions { weight: Weight 72.0 }
                         , MaxRepetitions { weight: Weight 80.0 } ]
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
