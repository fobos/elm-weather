module App exposing (..)

import Html exposing (Html, div, input, button, text, ul, li, p, label)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String
import Char
import Http
import Json.Decode as Json
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
    | NewWeather Location (Result Http.Error Float)
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

        NewWeather location (Ok temp) ->
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

        NewWeather _ (Err _) ->
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
            buildUrl "http://api.openweathermap.org/data/2.5/weather"
                [ ( "zip", zipCode ++ ",us" )
                , ( "units", "metric" )
                , ( "APPID", "cbbf2cace105ede143a9d7becd38400c" )
                ]
    in
        Http.send (NewWeather location) <|
            Http.get url decodeWeatherResponse


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


buildUrl : String -> List ( String, String ) -> String
buildUrl baseUrl args =
    case args of
        [] ->
            baseUrl

        _ ->
            baseUrl ++ "?" ++ String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    queryEscape key ++ "=" ++ queryEscape value


queryEscape : String -> String
queryEscape string =
    String.join "+" (String.split "%20" (Http.encodeUri string))
