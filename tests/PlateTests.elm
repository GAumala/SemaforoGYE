module PlateTests exposing (..)

import Test exposing (..)
import Expect
import Plate


-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Plate Test Suite"
        [ describe "Plate.fromString"
            [ test "it parses complete input" <|
                \_ ->
                    let
                        expected =
                            Just { letters = "GSB", numbers = "112", isEven = True }

                        actual =
                            Plate.fromString "GSB-112"
                    in
                        Expect.equal expected actual
            , test "it allows some whitespace" <|
                \_ ->
                    let
                        expected =
                            Just { letters = "GSB", numbers = "112", isEven = True }

                        actual =
                            Plate.fromString "GSB  112"
                    in
                        Expect.equal expected actual
            , test "it is case insensitive" <|
                \_ ->
                    let
                        expected =
                            Just { letters = "gsb", numbers = "112", isEven = True }

                        actual =
                            Plate.fromString "gsb - 112"
                    in
                        Expect.equal expected actual
            , test "it identifies if the number is odd" <|
                \_ ->
                    let
                        expected =
                            Just { letters = "GNA", numbers = "311", isEven = False }

                        actual =
                            Plate.fromString "GNA-311"
                    in
                        Expect.equal expected actual
            , test "Allows up to 4 digits" <|
                \_ ->
                    let
                        expected =
                            Just { letters = "GMN", numbers = "5249", isEven = False }

                        actual =
                            Plate.fromString "GMN-5249"
                    in
                        Expect.equal expected actual
            , test "Only needs the numbers portion to return Just" <|
                \_ ->
                    let
                        expected =
                            Just { letters = "", numbers = "620", isEven = True }

                        actual =
                            Plate.fromString " 620"
                    in
                        Expect.equal expected actual
            , test "Returns Nothing if input is invalid" <|
                \_ ->
                    let
                        actual =
                            Plate.fromString "6xyz"
                    in
                        Expect.equal Nothing actual
            ]
        , describe "Plate.print"
            [ test "it prints complete data" <|
                \_ ->
                    let
                        expected =
                            "GSB-621"

                        actual =
                            Plate.print
                                { letters = "GSB"
                                , numbers = "621"
                                , isEven = False
                                }
                    in
                        Expect.equal expected actual
            , test "it always prints uppercase" <|
                \_ ->
                    let
                        expected =
                            "GSB-633"

                        actual =
                            Plate.print
                                { letters = "gsb"
                                , numbers = "633"
                                , isEven = False
                                }
                    in
                        Expect.equal expected actual
            , test "it left pads missing letters" <|
                \_ ->
                    let
                        expected =
                            "XXX-621"

                        actual =
                            Plate.print
                                { letters = ""
                                , numbers = "621"
                                , isEven = False
                                }
                    in
                        Expect.equal expected actual
            ]
        ]
