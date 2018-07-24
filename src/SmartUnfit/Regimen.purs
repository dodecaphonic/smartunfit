module SmartUnfit.Regimen (Regimen, currentRegimen) where

import Prelude

import Data.Array (catMaybes, find)
import Data.Maybe (Maybe(..))
import SmartUnfit.Exercises (EquipmentAdjustment(..), Exercise, ExerciseDetails(..), ExerciseTechnique(..), MuscleGroup(..), Series, defaultExercises)

type Regimen = Array Series

adjustExercise :: String
               -> Array EquipmentAdjustment
               -> ExerciseTechnique
               -> Int
               -> Number
               -> Maybe String
               -> Maybe Exercise
adjustExercise name adj tec reps weight notes = adjust <$> exercise
  where
    exercise = find (\e -> e.description == name) defaultExercises
    adjust e =
      case e.details of
        (WeightTrainingExercise wte) ->
          e { notes = notes
            , technique = tec
            , details = WeightTrainingExercise
                          wte { equipmentAdjustments = adj
                              , weight = weight
                              , repeatTimes = reps
                          }
            }
        _ -> e

seriesA :: Series
seriesA =
  catMaybes $
    [ Just { description: "Remada Corda"
           , details: WeightTrainingExercise
                        { equipmentId: Nothing
                        , equipmentAdjustments: []
                        , weight: 18.25
                        , repeatTimes: 3
                        }
           , notes: Just "Uma perna à frente da outra"
           , muscleGroup: Shoulders
           , technique: Repetitions 12
           }
    , adjustExercise "Encolhimento"
                     []
                     (Repetitions 20) 3 9.0
                     (Just "HBC")
    , adjustExercise "Crucifixo Invertido Máquina"
                     [ NumericAdjustment 4 "Assento" ]
                     (Repetitions 12) 3 25.0
                     (Just "De frente para o assento. Prestar atenção no braço direito, e no ângulo dos braços ao abrir. Segurar 2\"")
    , adjustExercise "Lombar Máquina"
                     [ NumericAdjustment 1 "Apoio dos pés"
                     , NumericAdjustment 5 "Costas"
                     ]
                     (Repetitions 15) 3 40.0
                     (Just "Segurar 2\" em cada repetição")
    , adjustExercise "Rosca Alternada"
                     []
                     (RepetitionRange 10 12) 3 7.0
                     (Just "No banco 90° - HBC")
    , adjustExercise "Mesa Flexora"
                     [ NumericAdjustment 1 "Ângulo"
                     , DescriptiveAdjustment "XL" "Encaixe pés"]
                     (Repetitions 10) 3 20.0
                     Nothing
    , adjustExercise "Cadeira Abdutora"
                     []
                     (Repetitions 12) 3 90.0
                     (Just "Segurar 10\" ao final")
    , Just { description: "Abdominal Supra Apoiado na Bola"
           , details: BodyWeightExercise
                        { place: "Solo"
                        , repeatTimes: 3
                        }
           , muscleGroup: AbdomenLumbar
           , notes: Just "Peso 2kg no peito, bola de Pilates"
           , technique: RepetitionRange 25 35
           }
    , Just { description: "Perdigueiro"
           , details: BodyWeightExercise
                        { place: "Solo"
                        , repeatTimes: 3
                        }
           , muscleGroup: WholeBody
           , notes: Just "Pernas e braços alternados (braço direito estendido, perna esquerda estendida)."
           , technique: TimeHoldingPosture 30
           }
    , Just { description: "Esteira HIIT | Transport"
           , details: AerobicExercise { timeInMinutes: 20 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    ]

seriesB :: Series
seriesB =
  catMaybes $
    [ adjustExercise "Elevação Lateral"
                     []
                     (Repetitions 3) 10 5.0
                     (Just "PEG PRO/PEG NEU | HBC | Banco 90°")
    , Just { description: "Crucifixo Peito + Supino Inclinado"
            , details: WeightTrainingExercise
                       { equipmentId: Nothing
                       , equipmentAdjustments: []
                       , repeatTimes: 3
                       , weight: 7.0
                       }
            , muscleGroup: Pectoral
            , notes: Just "HBC, Banco 35°"
            , technique: Repetitions 12
            }
    , adjustExercise "Tríceps Polia"
                     []
                     (Repetitions 3) 12 8.65
                     (Just "Unilateral")
    , adjustExercise "Leg Press"
                     [ NumericAdjustment 9 "Assento" ]
                     (Repetitions 3) 12 50.0
                     Nothing
    , adjustExercise "Leg Press"
                     [ NumericAdjustment 9 "Assento" ]
                     (Repetitions 3) 12 20.0
                     (Just "Panturrilhas")
      # map (_ { description = "Leg Press (Panturrilha)" })
    , adjustExercise "Cadeira Extensora"
                     [ NumericAdjustment 6 "Assento"
                     , NumericAdjustment 11 "Ângulo"
                     , DescriptiveAdjustment "L" "Encaixe pés"
                     ]
                     (Repetitions 3) 12 22.5
                     Nothing
    , adjustExercise "Cadeira Adutora"
                     []
                     (Repetitions 3) 12 90.0
                     (Just "Segurar 10\" na última repetição")
    , Just { description: "Abdominal Infra Uni 90° Solo"
           , details: BodyWeightExercise
                        { repeatTimes: 3
                        , place: "Solo"
                        }
           , muscleGroup: AbdomenLumbar
           , notes: Nothing
           , technique: RepetitionRange 15 30
           }
    , Just { description: "Prancha Frontal"
           , details: BodyWeightExercise
                        { place: "Solo"
                        , repeatTimes: 3
                        }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: TimeHoldingPosture 15
           }
    , Just { description: "Prancha Lateral"
           , details: BodyWeightExercise
                        { place: "Solo"
                        , repeatTimes: 3
                        }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: TimeHoldingPosture 15
           }
    , Just { description: "Esteira HIIT | Transport"
           , details: AerobicExercise { timeInMinutes: 20 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    ]

currentRegimen :: Regimen
currentRegimen = [seriesA, seriesB]
