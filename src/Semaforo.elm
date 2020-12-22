module Semaforo exposing (isAllowed)

import Time exposing (Month(..), Posix, Weekday(..), Zone, toWeekday, toDay, toMonth)
import Plate exposing (Plate)


-- circulacion con semaforo en amarillo
-- Agosto:
-- https://www.eluniverso.com/sites/default/files/fotos/2020/07/circulacion-ago-2600.jpg
-- Septiembre:
-- https://twitter.com/ATMGuayaquil/status/1300858691571322881
-- Diciembre y Enero 2021:
-- https://twitter.com/ATMGuayaquil/status/1341182940412338178


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
                    allowedSundays plate.isEven zone date

                day =
                    toDay zone date
            in
                List.member day allowedDays

        _ ->
            False


allowedSundays : Bool -> Zone -> Posix -> List Int
allowedSundays isEven zone date =
    case toMonth zone date of
        Aug ->
            if isEven then
                [ 9, 23 ]
            else
                [ 2, 16, 30 ]

        Sep ->
            if isEven then
                [ 6, 20 ]
            else
                [ 13, 27 ]

        Dec ->
            if isEven then
                []
            else
                [ 27 ]

        Jan ->
            if isEven then
                [ 3 ]
            else
                []

        _ ->
            []


isAllowed : Plate -> Zone -> Posix -> Bool
isAllowed =
    august20Algorithm
