module Main exposing (..)

import Browser

import Element exposing (Element, el, text, column, row, alignRight, fill, px, width, rgb255, spacing, centerX, centerY, padding, height, paddingXY)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input

type alias Model = { count: Maybe Int, weight: Maybe Float, water: Maybe Float }
defaults =
  { count = 2
  , weight = 250.0
  , water = 57.0
  }

init : () -> ( Model, Cmd Msg )
init flags = ({ count = Just defaults.count, weight = Just defaults.weight, water = Just defaults.water }, Cmd.none)
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
      { model | count = Just (Maybe.withDefault defaults.count model.count + 1) }
    Decrement ->
      { model | count = Just (Maybe.withDefault defaults.count model.count - 1) }
    UpdateCount val ->
      { model | count = String.toInt val }
    UpdateWeight val ->
      { model | weight = String.toFloat val }
    UpdateWater val ->
      { model | water = String.toFloat val }

calcFlour model = ((toFloat (Maybe.withDefault defaults.count model.count)) * (Maybe.withDefault defaults.weight model.weight)) / (1 + (Maybe.withDefault defaults.water model.water)/100 + saltRatio + yeastRatio)
calcWater model = (calcFlour model) * (Maybe.withDefault defaults.water model.water) / 100
calcSalt  model = (calcFlour model) * saltRatio
calcYeast model = (calcFlour model) * yeastRatio

roundTo digits val = String.fromFloat (toFloat (Basics.round (val * 10^digits)) / 10^digits)

view model = Element.layout [] (column [ spacing 30, centerY, centerX, width (px 390) ]
  [
    row []
    [ Input.text [] { onChange = UpdateCount, text = Maybe.withDefault "" (Maybe.map String.fromInt model.count)   , label = Input.labelRight [] (text "very superbly delicious Pizza(s)"), placeholder = Just (Input.placeholder [] (text (String.fromInt defaults.count))) }
    , Input.button [ padding 5, spacing 5 ] { onPress = Just Decrement, label = text "-" }
    , Input.button [ padding 5, spacing 5 ] { onPress = Just Increment, label = text "+" }
    ]
  , Input.text [] { onChange = UpdateWeight,  text = Maybe.withDefault "" (Maybe.map String.fromFloat model.weight), label = Input.labelRight [] (text "grams (dough ball weight)"), placeholder = Just (Input.placeholder [] (text (String.fromFloat defaults.weight))) }
  , Input.text [] { onChange = UpdateWater ,  text = Maybe.withDefault "" (Maybe.map String.fromFloat model.water) , label = Input.labelRight [] (text "% water content (in bakers percents)"), placeholder = Just (Input.placeholder [] (text (String.fromFloat defaults.water))) }
  , column [ centerX, spacing 25, Font.size 32 ]
    [ el [ Font.italic, Font.size 28, centerX, paddingXY 0 20 ] (text "~~ Recipe ~~")
    , row [ Font.bold ] [ text (roundTo 0 (calcFlour model)), text " g" , text " Flour" ] 
    , row [ Font.bold ] [ text (roundTo 0 (calcWater model)), text " ml", text " Water" ]
    , row [ Font.bold ] [ text (roundTo 1 (calcSalt  model)), text " g" , text " Salt"  ]
    , row [ Font.bold ] [ text (roundTo 2 (calcYeast model)), text " g" , text " Yeast" ]
    ]
  ])
