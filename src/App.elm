module App exposing (..)

import Html exposing (Html, div, input, button, text, ul, li, p, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String
import Char
import Http
import Json.Decode as Json
import Task
import Time


-- MODEL


type alias Model =
    { zipInput : String
    , locations : List Location
    }


type alias Location =
    { zip : String
    , temp : Maybe Float
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" [], Cmd.none )



-- UPDATE


type Msg
    = AddLocation
    | ZipChange String
    | FetchSucceed Location Float
    | FetchFail Http.Error
    | UpdateWeather Time.Time


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
                        ( { model | zipInput = "" }
                        , fetchWeather (Location zipInput Nothing)
                        )
                    else
                        ( model, Cmd.none )
            in
                result

        ZipChange input ->
            ( { model | zipInput = String.filter Char.isDigit input }, Cmd.none )

        UpdateWeather _ ->
            let
                commands =
                    List.map fetchWeather model.locations
            in
                ( model, Cmd.batch commands )

        FetchSucceed location temp ->
            let
                updateLocation =
                    updateLocationTempByZip location.zip temp

                locations_ =
                    if List.member location model.locations then
                        List.map updateLocation model.locations
                    else
                        model.locations ++ [ { location | temp = Just temp } ]
            in
                ( { model | locations = locations_ }, Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )


updateLocationTempByZip : String -> Float -> Location -> Location
updateLocationTempByZip zipCode temp location =
    if location.zip == zipCode then
        { location | temp = Just temp }
    else
        location



-- HTTP


fetchWeather : Location -> Cmd Msg
fetchWeather location =
    let
        zipCode =
            location.zip

        url =
            Http.url "http://api.openweathermap.org/data/2.5/weather"
                [ ( "zip", zipCode ++ ",us" )
                , ( "units", "metric" )
                , ( "APPID", "cbbf2cace105ede143a9d7becd38400c" )
                ]
    in
        Task.perform FetchFail (FetchSucceed location) (Http.get decodeWeatherResponse url)


decodeWeatherResponse : Json.Decoder Float
decodeWeatherResponse =
    Json.at [ "main", "temp" ] Json.float



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if List.isEmpty model.locations then
        Sub.none
    else
        Time.every (20 * Time.second) UpdateWeather



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
        temp =
            case location.temp of
                Just temp ->
                    toString temp

                Nothing ->
                    "err"
    in
        div [ class "location", id ("zip-" ++ location.zip) ]
            [ p [ class "zip" ] [ text location.zip ]
            , p [ class "temp" ] [ text temp, text "℃" ]
            ]
