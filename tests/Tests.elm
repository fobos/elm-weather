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
                        expectedModel =
                            App.Model zipCode []

                        newModel =
                            (App.update
                                (App.ZipChange zipCode)
                                (App.Model "" [])
                            )
                                |> Tuple.first
                    in
                        expectedModel |> Expect.equal newModel
              --
            , fuzz Fuzz.string "filter only digit characters from input" <|
                \zipInput ->
                    let
                        numericStr =
                            String.filter Char.isDigit zipInput

                        expectedModel =
                            App.Model numericStr []

                        newModel =
                            (App.update
                                (App.ZipChange zipInput)
                                (App.Model "" [])
                            )
                                |> Tuple.first
                    in
                        expectedModel |> Expect.equal newModel
            ]
        , describe "Msg = AddLocation"
            [ fuzz invalidZipFuzzer "do not add empty or invalid zip code" <|
                \zipCode ->
                    let
                        model =
                            App.Model zipCode []

                        newModel =
                            App.update App.AddLocation model
                                |> Tuple.first
                    in
                        newModel |> Expect.equal model
            , test "add correct zip length =5 to state and perform fetch command" <|
                \() ->
                    let
                        newModel =
                            App.update App.AddLocation (App.Model "11111" [])
                                |> Tuple.first
                    in
                        -- I'm waiting for https://github.com/avh4/elm-testable/ release
                        -- to write tests on Cmds
                        newModel |> Expect.equal (App.Model "" [])
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
