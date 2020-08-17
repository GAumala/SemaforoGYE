module Semaforo exposing (isAllowed)

import Time exposing (Posix, Weekday(..), Zone, toWeekday, toDay)
import Plate exposing (Plate)


-- circulacion con semaforo en amarillo
-- https://www.eluniverso.com/sites/default/files/fotos/2020/07/circulacion-ago-2600.jpg


august20Algorithm : Plate -> Zone -> Posix -> Bool
august20Algorithm plate zone date =
    case ( toWeekday zone date, plate.isEven ) of
        ( Mon, False ) ->
            True

        ( Tue, True ) ->
            True

        ( Wed, False ) ->
            True

        ( Thu, True ) ->
            True

        ( Fri, False ) ->
            True

        ( Sat, True ) ->
            True

        ( Sun, isEven ) ->
            let
                allowedDays =
                    if isEven then
                        [ 9, 23 ]
                    else
                        [ 2, 16, 30 ]

                day =
                    toDay zone date
            in
                List.member day allowedDays

        _ ->
            False


isAllowed : Plate -> Zone -> Posix -> Bool
isAllowed =
    august20Algorithm
