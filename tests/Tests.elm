module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import App


all : Test
all =
    describe "A Test Suite"
        [ test "Pass" <|
            \() ->
                Expect.pass
        ]
