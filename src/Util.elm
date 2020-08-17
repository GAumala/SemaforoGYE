module Util exposing (readSystemTime, printReadableDate)

import Task exposing (Task)
import Time exposing (Month(..), Weekday(..))
import Tuple exposing (pair)


readSystemTime : Task x ( Time.Zone, Time.Posix )
readSystemTime =
    Task.map2 pair Time.here Time.now


printReadableDate : Time.Zone -> Time.Posix -> String
printReadableDate zone date =
    let
        month =
            case Time.toMonth zone date of
                Jan ->
                    "Enero"

                Feb ->
                    "Febrero"

                Mar ->
                    "Marzo"

                Apr ->
                    "Abril"

                May ->
                    "Mayo"

                Jun ->
                    "Junio"

                Jul ->
                    "Julio"

                Aug ->
                    "Agosto"

                Sep ->
                    "Septiembre"

                Oct ->
                    "Octubre"

                Nov ->
                    "Noviembre"

                Dec ->
                    "Diciembre"

        weekday =
            case Time.toWeekday zone date of
                Mon ->
                    "Lunes"

                Tue ->
                    "Martes"

                Wed ->
                    "Miércoles"

                Thu ->
                    "Jueves"

                Fri ->
                    "Viernes"

                Sat ->
                    "Sábado"

                Sun ->
                    "Domingo"

        day =
            String.fromInt <| Time.toDay zone date
    in
        weekday ++ " " ++ day ++ " de " ++ month
