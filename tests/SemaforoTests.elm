module SemaforoTests exposing (..)

import Test exposing (..)
import Expect
import Plate
import Semaforo exposing (isAllowed)
import Time exposing (millisToPosix, utc)


-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Semaforo Test Suite"
        [ test "it verifies correctly an even plate from Monday to Saturday" <|
            \_ ->
                let
                    expected =
                        [ False -- Monday
                        , True -- Tuesday
                        , False -- Wednesday
                        , True -- Thursday
                        , False -- Friday
                        , True -- Saturday
                        ]

                    timestamps =
                        [ 1596474000000 -- Monday
                        , 1596560400000 -- Tuesday
                        , 1596646800000 -- Wednesday
                        , 1596733200000 -- Thursday
                        , 1596819600000 -- Friday
                        , 1596906000000 -- Saturday
                        ]

                    posixValues =
                        List.map millisToPosix timestamps

                    placa =
                        { letters = "GSB", numbers = "112", isEven = True }

                    verifyAtDate =
                        isAllowed placa utc

                    actual =
                        List.map verifyAtDate posixValues
                in
                    Expect.equal expected actual
        , test "it verifies correctly an odd plate from Monday to Saturday" <|
            \_ ->
                let
                    expected =
                        [ True -- Monday
                        , False -- Tuesday
                        , True -- Wednesday
                        , False -- Thursday
                        , True -- Friday
                        , False -- Saturday
                        ]

                    timestamps =
                        [ 1596474000000 -- Monday
                        , 1596560400000 -- Tuesday
                        , 1596646800000 -- Wednesday
                        , 1596733200000 -- Thursday
                        , 1596819600000 -- Friday
                        , 1596906000000 -- Saturday
                        ]

                    posixValues =
                        List.map millisToPosix timestamps

                    placa =
                        { letters = "GSB", numbers = "021", isEven = False }

                    verifyAtDate =
                        isAllowed placa utc

                    actual =
                        List.map verifyAtDate posixValues
                in
                    Expect.equal expected actual
        , test "it verifies correctly an even plate on Sundays" <|
            \_ ->
                let
                    expected =
                        [ False -- Sunday 02
                        , True -- Sunday 09
                        , False -- Sunday 16
                        , True -- Sunday 23
                        , False -- Sunday 30
                        ]

                    timestamps =
                        [ 1596387600000 -- Sunday 02
                        , 1596992400000 -- Sunday 09
                        , 1597597200000 -- Sunday 16
                        , 1598202000000 -- Sunday 23
                        , 1598806800000 -- Sunday 30
                        ]

                    posixValues =
                        List.map millisToPosix timestamps

                    placa =
                        { letters = "GSB", numbers = "324", isEven = True }

                    verifyAtDate =
                        isAllowed placa utc

                    actual =
                        List.map verifyAtDate posixValues
                in
                    Expect.equal expected actual
        , test "it verifies correctly an odd plate on Sundays" <|
            \_ ->
                let
                    expected =
                        [ True -- Sunday 02
                        , False -- Sunday 09
                        , True -- Sunday 16
                        , False -- Sunday 23
                        , True -- Sunday 30
                        ]

                    timestamps =
                        [ 1596387600000 -- Sunday 02
                        , 1596992400000 -- Sunday 09
                        , 1597597200000 -- Sunday 16
                        , 1598202000000 -- Sunday 23
                        , 1598806800000 -- Sunday 30
                        ]

                    posixValues =
                        List.map millisToPosix timestamps

                    placa =
                        { letters = "GSB", numbers = "289", isEven = False }

                    verifyAtDate =
                        isAllowed placa utc

                    actual =
                        List.map verifyAtDate posixValues
                in
                    Expect.equal expected actual
        ]
