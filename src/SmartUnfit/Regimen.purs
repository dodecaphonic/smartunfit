module SmartUnfit.Regimen (Regimen, currentRegimen) where

import Prelude

import Data.Array (catMaybes, find)
import Data.Maybe (Maybe(..))
import SmartUnfit.Exercises (EquipmentAdjustment(..), Exercise, ExerciseDetails(..), ExerciseTechnique(..), MuscleGroup(..), RepetitionStyle(..), Series, TimeInSeconds(..), Weight(..), defaultExercises, mkRepetitionSequence)

type Regimen = Array Series

singleRep :: Number -> Int -> ExerciseTechnique
singleRep weight count
  = mkRepetitionSequence
      (Repetitions { weight: Weight weight, count })
      []

bodyWeightSingleRep :: Int -> ExerciseTechnique
bodyWeightSingleRep count
  = mkRepetitionSequence
    (Repetitions { weight: BodyWeight, count })
    []

repRange :: Number -> Int -> Int -> ExerciseTechnique
repRange weight minimum maximum
  = mkRepetitionSequence
      (RepetitionRange { weight: Weight weight, minimum, maximum })
      []

sequencedReps :: Number -> Int -> Int -> ExerciseTechnique
sequencedReps weight a b
  = mkRepetitionSequence
      (Repetitions { weight: Weight weight, count: a })
      [ Repetitions { weight: Weight weight, count: b } ]

timedRep :: Number -> Int -> ExerciseTechnique
timedRep weight howLong
  = mkRepetitionSequence
      (HoldPosture { weight: Weight weight, howLong: TimeInSeconds howLong })
      []

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
    [ Just { description: "Aquecimento (Bike)"
           , details: AerobicExercise { timeInMinutes: 10 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    , adjustExercise "Remada Máquina"
                     [ NumericAdjustment { adjustment: 6, description: "Banco" }
                     , NumericAdjustment { adjustment: 4, description: "Peito" }
                     ]
                     ( mkRepetitionSequence
                         (UnilateralRepetitions { weight: Weight 27.5, count: 12 })
                         [ Repetitions { weight: Weight 27.5, count: 12 }
                         ]
                     )
                     Nothing
    , adjustExercise "Agachamento Smith"
                     []
                     ( mkRepetitionSequence
                         (UnilateralRepetitions { weight: Weight 7.5, count: 5 })
                         [ Repetitions { weight: Weight 7.5, count: 10 }
                         ]
                     )
                     (Just "Afundo Unilateral + Bilateral")
    , Just { description: "Rosca com rotação (Bíceps)"
           , details: WeightTrainingExercise
                        { equipmentId: Nothing
                        , equipmentAdjustments: []
                        , seriesCount: 3
                        }
           , muscleGroup: Biceps
           , notes: Just "Banco em 90°\n\n40\"\n\30\"\n20\""
           , technique:
               mkRepetitionSequence
                 (MaxRepetitions { weight: Weight 9.0 })
                 [ MaxRepetitions { weight: Weight 8.0 }
                 , MaxRepetitions { weight: Weight 7.0 }
                 , MaxRepetitions { weight: Weight 6.0 }
                 ]
           }
    , Just { description: "Tiro na Bike"
           , details: AerobicExercise { timeInMinutes: 3 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    , adjustExercise "Puxada Polia Barra Frente"
           []
           ( mkRepetitionSequence
               (Repetitions { weight: Weight 26.0, count: 8 })
               [ Repetitions { weight: Weight 33.0, count: 8 }
               , Repetitions { weight: Weight 40.0, count: 8 }
               , MaxRepetitions { weight: Weight 45.0 }
               ]
           )
           (Just "Primeiras 3 pronadas, última supinada")
    , adjustExercise "Leg Press 45"
                     []
                     ( mkRepetitionSequence
                         (Repetitions { weight: Weight 30.0, count: 8 })
                         [ Repetitions { weight: Weight 30.0, count: 8 }
                         , Repetitions { weight: Weight 30.0, count: 12 }
                         ]
                     )
                     (Just "8x segurando 3\" entre reps, depois 8 normais, depois panturrilha.")
    , adjustExercise "Barra Fixa no Gravitron"
                     []
                     ( mkRepetitionSequence
                         (MaxRepetitions { weight: Weight 42.5 })
                         []
                     )
                     Nothing
    , Just { description: "Subida Lateral Alternada no Step"
           , details: BodyWeightExercise { place: "Step" }
           , muscleGroup: LowerLimbs
           , notes: Just "30\", 15\" de descanso entre reps"
           , technique: StayAtIt
           }
    , Just { description: "Abdominal"
           , details: AerobicExercise { timeInMinutes: 3 }
           , muscleGroup: LowerLimbs
           , notes: Just "Supra 30\"\nPedalada 30\"\nOblíquo 30\"\nInfra Uni 30\"\n2' de transport intenso entre séries."
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
    [ Just { description: "Aquecimento (Transport)"
           , details: AerobicExercise { timeInMinutes: 15 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    , adjustExercise "Supino Livre Inclinado"
                     []
                     ( mkRepetitionSequence
                         (Repetitions { weight: Weight 7.5, count: 10 })
                         []
                     )
                     (Just "Unilateral + Bilateral 2T")
    , adjustExercise "Cadeira Abdutora"
                     []
                     ( mkRepetitionSequence
                         (Repetitions { weight: Weight 72.5, count: 10 })
                         []
                     )
                     (Just "5 5T + 10 normais")
    , Just { description: "Tríceps Testa com HBM"
           , details: WeightTrainingExercise
                        { equipmentId: Nothing
                        , equipmentAdjustments: []
                        , seriesCount: 3
                        }
           , muscleGroup: Triceps
           , notes: Just "Descer a 90° na testa"
           , technique: singleRep 14.0 10
           }
    , Just { description: "Tiro no Transport"
           , details: AerobicExercise { timeInMinutes: 3 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    , Just { description: "Flexão de Braço Alternada no Step"
           , details: BodyWeightExercise { place: "Chão" }
           , muscleGroup: Triceps
           , notes: Nothing
           , technique:
               mkRepetitionSequence
                 (MaxRepetitions { weight: BodyWeight })
                 []
           }
    , adjustExercise "Elevação Frontal"
                     []
                     ( singleRep 5.0 10 )
                     (Just "Corda")
    , adjustExercise "Cadeira Flexora"
                     []
                     ( singleRep 65.0 16 )
                     (Just "Primeiras 8 segurando 3\" entre repetições")
    , Just { description: "Prancha na Bola"
           , details: BodyWeightExercise { place: "Bola de Pilates" }
           , muscleGroup: AbdomenLumbar
           , notes: Nothing
           , technique: TimeHoldingPosture (TimeInSeconds 30)
           }
    , Just { description: "Super-Homem"
           , details: BodyWeightExercise { place: "Colchonete" }
           , muscleGroup: AbdomenLumbar
           , notes: Nothing
           , technique: TimeHoldingPosture (TimeInSeconds 15)
           }
    , Just { description: "Prancha Lateral"
           , details: BodyWeightExercise { place: "Bosu" }
           , muscleGroup: AbdomenLumbar
           , notes: Nothing
           , technique: TimeHoldingPosture (TimeInSeconds 15)
           }
    , Just { description: "Aeróbico Livre (Esteira HIIT)"
           , details: AerobicExercise { timeInMinutes: 10 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    ]

currentRegimen :: Regimen
currentRegimen = [seriesA, seriesB]
