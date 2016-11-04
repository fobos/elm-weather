module App exposing (..)

import Html exposing (Html, div, input, button, text, ul, li, p, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String
import Char
import Http
import Json.Decode as Json
import Task


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
    | FetchSucceed String Float
    | FetchFail Http.Error


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
                            , locations = (locations ++ [ Location zipInput (Just "...") ])
                          }
                        , fetchWeather zipInput
                        )
                    else
                        ( model, Cmd.none )
            in
                result

        ZipChange input ->
            ( { model | zipInput = String.filter Char.isDigit input }, Cmd.none )

        FetchSucceed zipCode temp ->
            let
                updateLocation =
                    updateLocationTempByZip zipCode (toString temp)

                locations_ =
                    List.filterMap updateLocation model.locations
            in
                ( { model | locations = locations_ }, Cmd.none )

        FetchFail err ->
            case err of
                Http.UnexpectedPayload str ->
                    Debug.log str ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


updateLocationTempByZip : String -> String -> Location -> Maybe Location
updateLocationTempByZip zipCode temp location =
    if location.zip == zipCode then
        Just { location | temp = Just temp }
    else
        Just location



-- HTTP


fetchWeather : String -> Cmd Msg
fetchWeather zipCode =
    let
        url =
            Http.url "http://api.openweathermap.org/data/2.5/weather"
                [ ( "zip", zipCode ++ ",us" )
                , ( "units", "metric" )
                , ( "APPID", "cbbf2cace105ede143a9d7becd38400c" )
                ]
    in
        Task.perform FetchFail (FetchSucceed zipCode) (Http.get decodeWeatherResponse url)


decodeWeatherResponse : Json.Decoder Float
decodeWeatherResponse =
    Json.at [ "main", "temp" ] Json.float



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
            Maybe.withDefault "err" location.temp
    in
        div [ class "location", id ("zip-" ++ location.zip) ]
            [ p [ class "zip" ] [ text location.zip ]
            , p [ class "temp" ] [ text locationTemp, text "℃" ]
            ]
