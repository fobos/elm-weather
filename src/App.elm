module App exposing (..)

import Html exposing (Html, div, input, button, text, ul, li, p, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String
import Char


-- MODEL


type alias Model =
    { zipInput : String
    , locations : List Location
    }


type alias Location =
    { zip : String
    , temp : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" [], Cmd.none )



-- UPDATE


type Msg
    = AddLocation
    | ZipChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddLocation ->
            let
                { zipInput, locations } =
                    model

                containsZip =
                    List.any (\l -> l.zip == zipInput)

                needUpdateModel =
                    String.length zipInput == 5 && (not <| containsZip locations)

                result =
                    if needUpdateModel then
                        ( { model
                            | zipInput = ""
                            , locations = (locations ++ [ Location zipInput Nothing ])
                          }
                        , Cmd.none
                        )
                    else
                        ( model, Cmd.none )
            in
                result

        ZipChange input ->
            ( { model | zipInput = String.filter Char.isDigit input }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view { zipInput, locations } =
    div [ id "app-container" ]
        ([ div [ id "form" ]
            [ label [] [ text "Zip Code:" ]
            , input [ placeholder "Enter Zip", value zipInput, onInput ZipChange ] []
            , button [ onClick AddLocation ] [ text "Add location" ]
            ]
         ]
            ++ (List.map viewLocation locations)
        )


viewLocation : Location -> Html msg
viewLocation location =
    let
        locationTemp =
            Maybe.withDefault "0" location.temp
    in
        div [ class "location", id ("zip-" ++ location.zip) ]
            [ p [ class "zip" ] [ text location.zip ]
            , p [ class "temp" ] [ text locationTemp, text "℃" ]
            ]
