module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, label)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, id)

init =
  { count = 2
  , weight = 250.0
  , water = 57.0
  }
saltRatio = 0.03
yeastRatio = 0.002

main =
  Browser.sandbox { init = init, update = update, view = view }



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
      { model | count = Maybe.withDefault init.count (String.toInt val) }
    UpdateWeight val ->
      { model | weight = Maybe.withDefault init.weight (String.toFloat val) }
    UpdateWater val ->
      { model | water = Maybe.withDefault init.water (String.toFloat val) }

calcFlour model = ((toFloat model.count) * model.weight) / (1 + model.water/100 + saltRatio + yeastRatio)
calcWater model = (calcFlour model) * model.water / 100
calcSalt  model = (calcFlour model) * saltRatio
calcYeast model = (calcFlour model) * yeastRatio

roundTo digits val = String.fromFloat (toFloat (Basics.round (val * 10^digits)) / 10^digits)

view model =
  div []
    [
      input [ id "count", onInput UpdateCount, value (String.fromInt model.count) ] []
    , button [ id "countDec", onClick Decrement ] [ text "-" ]
    , button [ id "countInc", onClick Increment ] [ text "+" ]
    , input [ id "weight", onInput UpdateWeight, value (String.fromFloat model.weight) ] []
    , input [ id "waterPercent", onInput UpdateWater, value (String.fromFloat model.water) ] []
    , div [] [ text "flour: ", text (roundTo 0 (calcFlour model)), text " g" ]
    , div [] [ text "water: ", text (roundTo 0 (calcWater model)), text " ml" ]
    , div [] [ text "salt: ",  text (roundTo 1 (calcSalt model)) , text " g" ]
    , div [] [ text "yeast: ", text (roundTo 2 (calcYeast model)), text " g" ]
    ]
