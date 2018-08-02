module Main where

import Prelude

import App.Styles (styleSheet)
import App.Utils (groupBy, padLeft, seriesSeqToLabel)
import Data.Array (foldl, length, range, snoc, zip, (!!))
import Data.Const (Const)
import Data.Either (Either)
import Data.Foldable (oneOf)
import Data.Foldable as F
import Data.Generic.Rep as G
import Data.Generic.Rep.Show as GShow
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as S
import Data.Tuple (Tuple(..), uncurry)
import Effect (Effect)
import Routing (match)
import Routing.Hash (hashes)
import Routing.Match (Match, end, int, lit, root)
import SmartUnfit.Exercises (EquipmentAdjustment(..), Exercise, ExerciseDetails(..), ExerciseTechnique(..), MuscleGroup(..), Series)
import SmartUnfit.Regimen (Regimen, currentRegimen)
import Spork.App as App
import Spork.Html as H
import Spork.Interpreter (liftNat, merge, never)
import Web.DOM.Document (createElement, toParentNode)
import Web.DOM.Element (toNode)
import Web.DOM.Node (appendChild, setTextContent)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

data Action
  = ShowSeriesSelector
  | SelectSeries Int
  | SelectExercise Int Int

derive instance genericAction :: G.Generic Action _

instance showAction :: Show Action where
  show = GShow.genericShow

data SmartUnfitEffect a =
  LoadFromStorage a

newtype Model =
  Model
    { regimen :: Regimen
    , selectedSeries :: Maybe Int
    , selectedExercise :: Maybe Int
    }

route :: Match Action
route =
  oneOf
    [ ShowSeriesSelector <$ end
    , root *> (SelectSeries <$> (lit "series" *> int) <* end)
    , root *>
        (SelectExercise <$>
          (lit "series" *> int) <*>
          (lit "exercises" *> int) <*
          end
        )
    ]

routeAction :: String -> Either String Action
routeAction = match route

init :: Model
init =
  Model
    { regimen: currentRegimen
    , selectedSeries: Nothing
    , selectedExercise: Nothing
    }

runSmartUnfitEffect :: SmartUnfitEffect ~> Effect
runSmartUnfitEffect = case _ of
  LoadFromStorage next ->
    pure next

update :: Model -> Action -> App.Transition SmartUnfitEffect Model Action
update (Model model) = case _ of
  ShowSeriesSelector ->
    App.purely $
      Model $
      model { selectedExercise = Nothing, selectedSeries = Nothing}

  SelectSeries idx ->
    App.purely $ Model $ model { selectedSeries = Just idx }

  SelectExercise series exercise ->
    App.purely $
      Model $
        model { selectedSeries = Just series
              , selectedExercise = Just exercise }

seriesSelectorView :: Int -> H.Html Action
seriesSelectorView i =
  H.a
    [ H.classes [ "series-selection" ]
    , H.href ("/#/series/" <> show i)
    ]
    [ H.text $ seriesSeqToLabel i ]

selectSeriesView :: Regimen -> H.Html Action
selectSeriesView regimen =
  H.div
    [ H.classes [ "series-selector" ] ]
    (map seriesSelectorView (range 0 $ length regimen - 1))

noSelectedSeriesView :: H.Html Action
noSelectedSeriesView = H.div [] [ H.text "Fudge" ]

seriesView :: Int -> Series -> H.Html Action
seriesView seriesIdx series =
  H.div
    []
    ([ H.h1
       [ H.classes [ "series" ] ]
       [ H.text (seriesSeqToLabel seriesIdx) ]
     ] <> map (uncurry $ groupView seriesIdx) grouped)
  where
    grouped = Map.toUnfoldable $ groupBy (\(Tuple i e) -> e.muscleGroup) indexedSeries
    indexedSeries = zip (range 0 $ length series) series

groupView :: Int -> MuscleGroup -> Array (Tuple Int Exercise) -> H.Html Action
groupView seriesIdx mg tme =
  H.div
    [ H.classes [ "exercise-group" ] ]
    [ H.div
      [ H.classes [ "name" ] ]
      [ H.text (muscleGroupName mg) ]
    , H.div
        [ H.classes [ "exercises" ] ]
        (map (uncurry $ exerciseListItemView seriesIdx) tme)
    ]
  where
    muscleGroupName = case _ of
      Dorsal -> "Dorsal"
      Biceps -> "Bíceps"
      Triceps -> "Tríceps"
      Pectoral -> "Peitoral"
      LowerLimbs -> "Membros Inferiores"
      Shoulders -> "Ombros"
      AbdomenLumbar -> "Abdominal / Lombar"
      WholeBody -> "Corpo Inteiro"

exerciseSummaryView :: Exercise -> H.Html Action
exerciseSummaryView ex =
  H.div
    [ H.classes [ "exercise" ] ]
    [ H.div
        [ H.classes [ "equipment-id", muscleGroupClassName ] ]
        [ H.text equipmentId ]
    , H.div
        [ H.classes [ "details" ] ]
        [ H.div [ H.classes [ "name" ] ] [ H.text ex.description ]
        , H.div
            [ H.classes [ "adjustments" ] ]
            [ H.div
              [ H.classes [ "settings" ] ]
              [ H.text adjustmentsAsString ]
            ]
        ]
    , H.div
        [ H.classes [ "important" ] ]
        [ H.text important ]
    ]
  where
    adjustmentsAsString =
      case ex.details of
        (WeightTrainingExercise d) ->
          S.joinWith "/" $ foldl addAdjustment [] d.equipmentAdjustments
        _ -> ""
      where
        addAdjustment acc (NumericAdjustment i s) =
          snoc acc $ (show i) <> " (" <> s <> ")"
        addAdjustment acc (DescriptiveAdjustment s d) =
          snoc acc (s <> " (" <> d <> ")")
    important =
      case ex.details of
        (WeightTrainingExercise e) -> (show e.weight) <> "kg"
        (AerobicExercise e) -> (show e.timeInMinutes) <> "'"
        (BodyWeightExercise e) ->
          case ex.technique of
            (TimeHoldingPosture t) -> (show t) <> "\""
            (Repetitions i) -> show i
            (RepetitionRange s f) -> show s <> "-" <> show f
            _ -> ""

    muscleGroupClassName =
       case ex.muscleGroup of
         Dorsal -> "torso"
         Biceps -> "arms"
         Triceps -> "arms"
         Pectoral -> "torso"
         LowerLimbs -> "lower-limbs"
         Shoulders -> "arms"
         AbdomenLumbar -> "abdomen-lumbar"
         WholeBody -> "body"

    equipmentId = case ex.details of
      (WeightTrainingExercise wte) ->
        fromMaybe "" $ padLeft 2 "0" <<< show <$> wte.equipmentId
      _ -> ""

exerciseListItemView :: Int -> Int -> Exercise -> H.Html Action
exerciseListItemView seriesIdx exerciseIdx ex =
  H.a
    [ H.href ("/#/series/" <> show seriesIdx <> "/exercises/" <> show exerciseIdx)
    , H.classes ["exercise-list-item"]
    ]
    [ exerciseSummaryView ex ]


exerciseView :: Regimen -> Int -> Int -> H.Html Action
exerciseView regimen seriesIdx exerciseIdx =
  H.div
    []
    [ H.h1
      [ H.classes [ "series" ] ]
      [ H.text (seriesSeqToLabel seriesIdx) ]
    , fromMaybe missingExerciseDetailsView $ exerciseDetailsView <$> exercise
    ]
  where
    exercise =
      regimen !! seriesIdx >>= \series -> series !! exerciseIdx

exerciseDetailsView :: Exercise -> H.Html Action
exerciseDetailsView exercise =
  H.div
    []
    [ exerciseSummaryView exercise
    , exerciseExtraInfoView exercise
    ]

exerciseExtraInfoView :: Exercise -> H.Html Action
exerciseExtraInfoView exercise =
  H.div
    []
    [ H.text details
    , H.br []
    , H.text technique
    , H.br []
    , H.text (fromMaybe "" exercise.notes)
    ]
  where
    details =
      case exercise.details of
        (WeightTrainingExercise wte) ->
          "Séries: " <> show wte.repeatTimes
        otherwise -> ""

    technique =
      case exercise.technique of
        (Repetitions n) ->
          "Repetições: " <> show n
        (RepetitionRange f t) ->
          "Repetições: " <> show f <> "-" <> show t
        StayAtIt ->
          ""
        (TimeHoldingPosture n) ->
          "Segurar por " <> show n <> "\""

missingExerciseDetailsView :: H.Html Action
missingExerciseDetailsView =
  H.div [] [ H.text "Ops! Exercício faltando." ]

render :: Model -> H.Html Action
render (Model model) =
  case model.selectedSeries of
    Just series ->
      case model.selectedExercise of
        Just exercise ->
          exerciseView model.regimen series exercise
        Nothing ->
          maybe
            noSelectedSeriesView
            (seriesView series)
            (model.regimen !! series)
    Nothing ->
      selectSeriesView model.regimen

app :: App.App SmartUnfitEffect (Const Void) Model Action
app =
  { update
  , render
  , init: App.purely init
  , subs: const mempty
  }

addStylesheet :: Effect Unit
addStylesheet = do
  doc <- document =<< window

  let docNode = toDocument doc
  mHead <- querySelector (QuerySelector "head") $ toParentNode docNode

  case mHead of
    Just head -> do
      style <- createElement "style" docNode
      let styleNode = toNode style
      _ <- setTextContent styleSheet styleNode

      void $ appendChild styleNode (toNode head)
    Nothing ->
      pure unit

main :: Effect Unit
main = do
  addStylesheet

  inst <-
    App.makeWithSelector
      (liftNat runSmartUnfitEffect `merge` never)
      app
      "#app"

  inst.run

  void $ hashes \oldHash newHash -> do
    F.for_ (routeAction newHash) \i -> do
      inst.push i
      inst.run
