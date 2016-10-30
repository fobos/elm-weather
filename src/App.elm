module App exposing (..)

import Html exposing (Html, div, input, button, text, ul, li, p, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex exposing (regex, contains)


-- MODEL

type alias Model =
     List Location

type alias Weather =
     String

type alias Location =
    { zip : String
    , temp : Maybe Weather
    }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- UPDATE

type Msg
    = AddLocation
    | ZipChange String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AddLocation ->
            ( [], Cmd.none )

        ZipChange zip ->
            if contains (regex "^\\d{5}$") zip then
                ( { zip = zip, temp = Nothing } :: model, Cmd.none)
            else
                ( model, Cmd.none )

-- INIT

init : (Model, Cmd Msg)
init =
    ( [], Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    div [ id "app-container" ]
        ([ div [ id "form" ]
            [ label [] [ text "Zip Code:"]
            , input [ placeholder "Enter Zip",  onInput ZipChange] []
            , button [ onClick AddLocation ] [ text "Add location" ]
            ]
        ] ++ (List.map viewLocation model))

viewLocation : Location -> Html msg
viewLocation location =
    let
        locationTemp = Maybe.withDefault "0" location.temp
    in
        div [ class "location", id ("zip-" ++ location.zip) ]
            [ p [ class "zip" ] [ text location.zip ]
            , p [ class "temp" ] [ text locationTemp, text "℃"]
            ]
