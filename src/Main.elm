module Main exposing (..)

import Browser

import Element exposing (Element, el, text, column, row, alignRight, fill, px, width, rgb255, spacing, centerX, centerY, padding, height, paddingXY)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input

type alias Model = { count: Int, weight: Float, water: Float }
defaults: Model
defaults =
  { count = 2
  , weight = 250.0
  , water = 57.0
  }

init : () -> ( Model, Cmd Msg )
init flags = (defaults, Cmd.none)
saltRatio = 0.03
yeastRatio = 0.002

main =
  Browser.document { init = init, update = updateNoCmd, view = doc, subscriptions = noSubs }

doc model =
  { title = "Pizza Recipe"
  , body = [view model]
  }
updateNoCmd msg model = (update msg model, Cmd.none)
noSubs model = Sub.none


type Msg = Increment
         | Decrement
         | UpdateCount String
         | UpdateWeight String
         | UpdateWater String

update msg model =
  case msg of
    Increment ->
      { model | count = model.count + 1 }
    Decrement ->
      { model | count = model.count - 1 }
    UpdateCount val ->
      { model | count = Maybe.withDefault defaults.count (String.toInt val) }
    UpdateWeight val ->
      { model | weight = Maybe.withDefault defaults.weight (String.toFloat val) }
    UpdateWater val ->
      { model | water = Maybe.withDefault defaults.water (String.toFloat val) }

calcFlour model = ((toFloat model.count) * model.weight) / (1 + model.water/100 + saltRatio + yeastRatio)
calcWater model = (calcFlour model) * model.water / 100
calcSalt  model = (calcFlour model) * saltRatio
calcYeast model = (calcFlour model) * yeastRatio

roundTo digits val = String.fromFloat (toFloat (Basics.round (val * 10^digits)) / 10^digits)

view model = Element.layout [] (column [ spacing 30, centerY, centerX, width (px 390) ]
  [
    row []
    [ Input.text [] { onChange = UpdateCount , text = String.fromInt model.count   , label = Input.labelRight [] (text "very superbly delicious Pizza(s)"), placeholder = Maybe.Nothing }
    , Input.button [ padding 5, spacing 5 ] { onPress = Just Decrement, label = text "-" }
    , Input.button [ padding 5, spacing 5 ] { onPress = Just Increment, label = text "+" }
    ]
  , Input.text [] { onChange = UpdateWeight, text = String.fromFloat model.weight, label = Input.labelRight [] (text "grams (dough ball weight)"), placeholder = Maybe.Nothing }
  , Input.text [] { onChange = UpdateWater , text = String.fromFloat model.water , label = Input.labelRight [] (text "% water content (in bakers percents)"), placeholder = Maybe.Nothing }
  , column [ centerX, spacing 25, Font.size 32 ]
    [ el [ Font.italic, Font.size 28, centerX, paddingXY 0 20 ] (text "~~ Recipe ~~")
    , row [ Font.bold ] [ text (roundTo 0 (calcFlour model)), text " g" , text " Flour" ] 
    , row [ Font.bold ] [ text (roundTo 0 (calcWater model)), text " ml", text " Water" ]
    , row [ Font.bold ] [ text (roundTo 1 (calcSalt  model)), text " g" , text " Salt"  ]
    , row [ Font.bold ] [ text (roundTo 2 (calcYeast model)), text " g" , text " Yeast" ]
    ]
  ])
