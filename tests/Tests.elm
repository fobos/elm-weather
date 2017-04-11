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
                        Expect.equal expectedModel newModel
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
                        Expect.equal expectedModel newModel
              --
            , test "Passed test" <|
                \() -> Expect.pass
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


numberGenerator : Generator Char
numberGenerator =
    (Random.map Char.fromCode (Random.int 48 57))


lengthString : Generator Char -> Int -> Generator String
lengthString charGenerator stringLength =
    Random.list stringLength charGenerator
        |> Random.map String.fromList
