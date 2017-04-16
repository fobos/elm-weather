module Tests exposing (..)

import Test exposing (..)
import Fuzz exposing (Fuzzer)
import Expect
import Random.Pcg as Random exposing (Generator)
import Shrink
import String
import Char
import App


all : Test
all =
    describe "App.update"
        [ describe "Msg = ZipChange"
            [ fuzz zipFuzzer "works with various length zip codes" <|
                \zipCode ->
                    let
                        msg =
                            App.ZipChange zipCode
                    in
                        App.update msg (App.Model "" [])
                            |> Tuple.first
                            |> Expect.equal (App.Model zipCode [])
              --
            , fuzz Fuzz.string "filter only digit characters from input" <|
                \zipInput ->
                    let
                        numericStr =
                            String.filter Char.isDigit zipInput

                        msg =
                            App.ZipChange zipInput
                    in
                        App.update msg (App.Model "" [])
                            |> Tuple.first
                            |> Expect.equal (App.Model numericStr [])
            ]
        , describe "Msg = AddLocation"
            [ fuzz invalidZipFuzzer "do not add empty or invalid zip code" <|
                \zipCode ->
                    let
                        model =
                            App.Model zipCode []
                    in
                        App.update App.AddLocation model
                            |> Tuple.first
                            |> Expect.equal model
            , test "add correct zip length=5 to state and perform fetch command" <|
                \() ->
                    -- I'm waiting for https://github.com/avh4/elm-testable/ release
                    -- to write tests on Cmds
                    App.update App.AddLocation (App.Model "11111" [])
                        |> Tuple.first
                        |> Expect.equal (App.Model "" [])
            ]
        , describe "Msg = Update Weather"
            [ test "create list of Cmd to update weather" <|
                -- TODO: just after elm-testable relesae
                \() -> Expect.pass
            ]
        , describe "Msg = NewWeater"
            [ test "add new location with fetched temp to state" <|
                \() ->
                    let
                        newLocation =
                            App.Location "11111" Nothing

                        msg =
                            App.NewWeather newLocation (Ok 10)
                    in
                        App.update msg (App.Model "" [])
                            |> Tuple.first
                            |> Expect.all
                                [ \{ zipInput, locations } ->
                                    zipInput |> Expect.equal ""
                                , \{ zipInput, locations } ->
                                    locations |> Expect.equalLists [ App.Location "11111" (Just 10) ]
                                ]
            , test "update existed location with new temp" <|
                \() ->
                    let
                        location =
                            App.Location "11111" (Just 10)

                        msg =
                            App.NewWeather location (Ok 20)
                    in
                        App.update msg (App.Model "" [ location ])
                            |> Tuple.first
                            |> Expect.all
                                [ \{ zipInput, locations } ->
                                    zipInput |> Expect.equal ""
                                , \{ zipInput, locations } ->
                                    locations |> Expect.equalLists [ App.Location "11111" (Just 20) ]
                                ]
            ]
        ]


zip5Fuzzer : Fuzzer String
zip5Fuzzer =
    Fuzz.custom
        (Random.constant 5
            |> Random.andThen (lengthString numberGenerator)
        )
        Shrink.string


zipFuzzer : Fuzzer String
zipFuzzer =
    let
        generator : Generator String
        generator =
            Random.frequency
                [ ( 0.7, Random.int 1 10 )
                , ( 1, Random.constant 5 )
                , ( 0.1, Random.constant 0 )
                ]
                |> Random.andThen (lengthString numberGenerator)
    in
        Fuzz.custom generator Shrink.string


invalidZipFuzzer : Fuzzer String
invalidZipFuzzer =
    let
        generator : Generator String
        generator =
            Random.frequency
                [ ( 0.5, Random.int 1 4 )
                , ( 0.5, Random.int 6 10 )
                , ( 0.1, Random.constant 0 )
                ]
                |> Random.andThen (lengthString numberGenerator)
    in
        Fuzz.custom generator Shrink.string


numberGenerator : Generator Char
numberGenerator =
    (Random.map Char.fromCode (Random.int 48 57))


lengthString : Generator Char -> Int -> Generator String
lengthString charGenerator stringLength =
    Random.list stringLength charGenerator
        |> Random.map String.fromList
