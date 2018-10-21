module SmartUnfit.Regimen (Regimen, currentRegimen) where

import Prelude

import Data.Array (catMaybes, find)
import Data.Maybe (Maybe(..))
import SmartUnfit.Exercises ( EquipmentAdjustment(..)
                            , Exercise
                            , ExerciseDetails(..)
                            , ExerciseTechnique(..)
                            , MuscleGroup(..)
                            , RepetitionStyle(..)
                            , Series
                            , TimeInSeconds(..)
                            , Weight(..)
                            , defaultExercises
                            , mkRepetitionSequence
                            )

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
           , details: AerobicExercise { timeInMinutes: 15 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    , adjustExercise "Remada Alta"
                     []
                     ( singleRep 22.5 15 )
                     (Just "Barra W na Polia\n2 tempos")
    , adjustExercise "Rosca Direta"
                     []
                     ( mkRepetitionSequence
                         (HoldPosture { weight: Weight 22.5, howLong: TimeInSeconds 10 })
                         [ Repetitions { weight: Weight 22.5, count: 12 }
                         ]
                     )
                     (Just "Barra W na Polia")
    , adjustExercise "Crucifixo Invertido Máquina"
                     [ NumericAdjustment { adjustment: 6, description: "Banco" }
                     , NumericAdjustment { adjustment: 0, description: "Amplitude" }
                     ]
                     ( mkRepetitionSequence
                         (UnilateralRepetitions { weight: Weight 20.0, count: 8 })
                         [ Repetitions { weight: Weight 35.0, count: 10 }
                         ]
                     )
                     Nothing
    , Just { description: "Sobe e Desce na Escada"
           , details: AerobicExercise { timeInMinutes: 1 }
           , muscleGroup: LowerLimbs
           , notes: Nothing
           , technique: StayAtIt
           }
    , adjustExercise "Puxada Polia Barra"
                     []
                     ( mkRepetitionSequence
                         (HoldPosture { weight: Weight 47.0, howLong: TimeInSeconds 3 })
                         [ Repetitions { weight: Weight 47.0, count: 8 }
                         ]
                     )
                     (Just "8x segurando 3\" entre reps, depois 8 normais\nBarra triangular")
    , Just { description: "Bíceps com Barra H"
           , details: WeightTrainingExercise
                        { equipmentId: Nothing
                        , equipmentAdjustments: []
                        , seriesCount: 3
                        }
           , muscleGroup: Biceps
           , notes: Just "Preparar antes de fazer Puxada Polia"
           , technique: mkRepetitionSequence (MaxRepetitions { weight: Weight 2.5 }) []
           }
    , adjustExercise "Agachamento Smith"
                     []
                     ( mkRepetitionSequence
                         (Repetitions { weight: Weight 2.5, count: 10 })
                         [ HoldPosture { weight: Weight 2.5, howLong: TimeInSeconds 5 }
                         ]
                     )
                     Nothing
    , adjustExercise "Afundo"
                     []
                     ( singleRep 5.0 10 )
                     (Just "Com anilha.\n2 tempos, alternando pernas")
    , Just { description: "Panturrilha na Escada"
           , details: BodyWeightExercise { place: "Escada" }
           , muscleGroup: LowerLimbs
           , notes: Just "Segurar 2\" na subida"
           , technique: bodyWeightSingleRep 15
           }
    , Just { description: "Corrida no Bosu"
           , details: AerobicExercise { timeInMinutes: 3 }
           , muscleGroup: LowerLimbs
           , notes: Just "Correr 30\", descansar 15\", repetir"
           , technique: StayAtIt
           }
    , Just { description: "Abdominal no Step"
           , details: BodyWeightExercise { place: "Step" }
           , muscleGroup: AbdomenLumbar
           , notes: Nothing
           , technique: mkRepetitionSequence
                          (MaxRepetitions { weight: BodyWeight })
                          []
           }
    , Just { description: "Abdominal no Bosu com Anilha"
           , details: BodyWeightExercise { place: "Step" }
           , muscleGroup: AbdomenLumbar
           , notes: Nothing
           , technique: mkRepetitionSequence
                          (MaxRepetitions { weight: Weight 5.0 })
                          []
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
    , adjustExercise "Supino Máquina"
                     [ NumericAdjustment { adjustment: 7, description: "Assento" }
                     , NumericAdjustment { adjustment: 2, description: "Braços" } ]
                     ( mkRepetitionSequence
                         (Repetitions { weight: Weight 20.0, count: 8 })
                         [ MaxRepetitions { weight: Weight 30.0 }
                         ]
                     )
                     (Just "Unilateral + Bilateral 2T")
    , adjustExercise "Mesa Flexora"
                     []
                     ( mkRepetitionSequence
                         (Repetitions { weight: Weight 12.5, count: 10 })
                         [ MaxRepetitions { weight: Weight 20.0 }
                         ]
                     )
                     (Just "Unilateral + Bilateral\nMovimento concentrado na descida")
    , adjustExercise "Tríceps Polia"
                     []
                     ( mkRepetitionSequence
                         (Repetitions { weight: Weight 15.0, count: 8 })
                         [ Repetitions { weight: Weight 20.0, count: 8 }
                         , Repetitions { weight: Weight 25.0, count: 8 }
                         , MaxRepetitions { weight: Weight 20.0 }
                         ]
                     )
                     (Just "Pronado + Supinado")
    , Just { description: "Tiro no Transport"
           , details: AerobicExercise { timeInMinutes: 3 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    , adjustExercise "Pullover"
                     []
                     ( mkRepetitionSequence
                         (Repetitions { weight: Weight 7.5, count: 12 })
                         [ HoldPosture { weight: Weight 7.5, howLong: TimeInSeconds 10 }
                         ]
                     )
                     (Just "Crucifixo\nPolia Média (8)")
    , adjustExercise "Cadeira Abdutora"
                     []
                     ( singleRep 65.0 16 )
                     (Just "Primeiras 8 segurando 3\" entre repetições")
    , adjustExercise "Elevação Frontal"
                     []
                     (singleRep 5.0 10)
                     (Just "Alternado, HBC")
    , adjustExercise "Elevação Lateral"
                     []
                     (singleRep 5.0 10)
                     (Just "Simultâneo, HBC")
    , Just { description: "Polichinelos"
           , details: AerobicExercise { timeInMinutes: 3 }
           , muscleGroup: WholeBody
           , notes: Just "30\"/15\""
           , technique: StayAtIt
           }
    , Just { description: "Prancha Lateral"
           , details: BodyWeightExercise { place: "Colchonete" }
           , muscleGroup: AbdomenLumbar
           , notes: Nothing
           , technique: TimeHoldingPosture (TimeInSeconds 15)
           }
    , Just { description: "Perdigueiro"
           , details: BodyWeightExercise { place: "Colchonete" }
           , muscleGroup: AbdomenLumbar
           , notes: Nothing
           , technique: TimeHoldingPosture (TimeInSeconds 15)
           }
    , Just { description: "Aeróbico Livre (Esteira HIIT)"
           , details: AerobicExercise { timeInMinutes: 15 }
           , muscleGroup: WholeBody
           , notes: Nothing
           , technique: StayAtIt
           }
    ]

currentRegimen :: Regimen
currentRegimen = [seriesA, seriesB]
