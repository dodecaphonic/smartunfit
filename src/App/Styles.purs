module App.Styles (css, styleSheet) where

import Prelude

import CSS (CSS, Prefixed(..), Value(..), alignItems, alignSelf, backgroundColor, borderBottom, color, column, display, em, flex, flexDirection, flexStart, fontSize, fromString, height, justifyContent, key, letterSpacing, margin, marginBottom, marginLeft, marginTop, minHeight, paddingBottom, paddingTop, pct, px, renderedSheet, rgb, row, solid, textDecoration, value, white, width, (?))
import CSS.Common (none)
import CSS.Common as CSS
import CSS.Render (render)
import CSS.Text (TextDecoration(..))
import CSS.Text.Transform (textTransform, uppercase)
import CSS.TextAlign (TextAlign(..), center, textAlign)
import Data.Maybe (maybe)

css :: CSS
css = do
  let green = rgb 14 196 172
      blue = rgb 14 154 196
      orange = rgb 255 185 0
      armsEquipment = rgb 0 191 63
      torsoEquipment = rgb 255 132 0
      lowerLimbsEquipment = rgb 49 79 137
      abdomenLumbarEquipment = rgb 252 0 0
      darkGrey = rgb 31 33 35

  fromString "body" ? do
    backgroundColor darkGrey
    key (fromString "font-family") (value "-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen-Sans,Ubuntu,Cantarell,\"Helvetica Neue\",sans-serif")
    color white
    fontSize (1.2 #em)

  fromString "h1" ? do
    fontSize (48.0 #px)
    marginTop (48.0 #px)
    textTransform uppercase
    letterSpacing (6.0 #px)

  fromString "h1.series" ? do
    color white
    height (60.0 #px)
    fontSize (2.0 #em)
    backgroundColor orange
    borderBottom solid (1.0 #px) white
    textAlign center
    margin (0.0 #px) (0.0 #px) (0.0 #px) (0.0 #px)

  fromString "h1.series:not(eq(1))" ? do
    marginTop (30.0 #px)

  fromString ".exercise" ? do
    minHeight (80.0 #px)
    display flex
    alignItems CSS.center
    marginTop (25.0 #px)
    borderBottom solid (1.0 #px) (rgb 224 224 224)

  fromString ".exercise .details" ? do
    alignSelf flexStart
    marginLeft (0.5 #em)
    width (100.0 #pct)

  fromString ".exercise .details .name" ? do
    fontSize (1.2 #em)

  fromString ".exercise .details .adjustments" ? do
    display flex
    flexDirection row
    marginTop (15.0 #px)
    marginBottom (15.0 #px)
    fontSize (0.8 #em)
    width (100.0 #pct)

  fromString ".exercise .details .adjustments .settings" ? do
    color (rgb 158 158 158)

  fromString ".exercise .important" ? do
    alignSelf flexStart
    fontSize (1.2 #em)
    color (rgb 97 97 97)
    textAlign (TextAlign (Value (Plain "right")))

  fromString ".exercise .equipment-id" ? do
    fontSize (2.0 #em)
    alignSelf flexStart
    color orange
    width (120.0 #px)
    textAlign center

  fromString ".exercise-list-item" ? do
    color white
    textDecoration (TextDecoration none)

  fromString ".exercise .equipment-id.arms" ? do
    backgroundColor armsEquipment
    color white

  fromString ".exercise .equipment-id.torso" ? do
    backgroundColor torsoEquipment
    color white

  fromString ".exercise .equipment-id.lower-limbs" ? do
    backgroundColor lowerLimbsEquipment
    color white

  fromString ".exercise .equipment-id.abdomen-lumbar" ? do
    backgroundColor abdomenLumbarEquipment
    color white

  fromString ".exercise-group" ? do
    marginTop (30.0 #px)
    marginBottom (30.0 #px)

  fromString ".exercise-group > .name" ? do
    backgroundColor (rgb 62 62 62)
    color white
    fontSize (1.0 #em)
    paddingTop (5.0 #px)
    paddingBottom (5.0 #px)
    textAlign center
    marginBottom (15.0 #px)
    textTransform uppercase

  fromString ".exercise-group .exercises" ? do
    margin (0.0 #px) (10.0 #px) (15.0 #px) (0.0 #px)

  fromString ".series-selector" ? do
    display flex
    flexDirection column
    width (100.0 #pct)
    height (100.0 #pct)
    alignItems CSS.center

  fromString ".series-selector > .series-selection" ? do
    backgroundColor orange
    color white
    fontSize (5.0 #em)
    width (50.0 #pct)
    height (200.0 #px)
    display flex
    alignItems CSS.center
    justifyContent CSS.center
    textDecoration (TextDecoration none)

  fromString ".series-selection:not(:first-child)" ? do
    marginTop (30.0 #px)

styleSheet :: String
styleSheet =
  maybe "" identity (renderedSheet $ render css)
