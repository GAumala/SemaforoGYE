module Plate exposing (Plate, fromString, print)

import Regex


type alias Plate =
    { letters : String, numbers : String, isEven : Bool }


regexOpts =
    { caseInsensitive = True, multiline = False }


numbersRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith regexOpts "[0-9]{3,4}$"


lettersRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith regexOpts "^[A-Z]{3}"


endsWithEven : String -> Bool
endsWithEven input =
    case String.right 1 input of
        "0" ->
            True

        "2" ->
            True

        "4" ->
            True

        "6" ->
            True

        "8" ->
            True

        _ ->
            False


fromString : String -> Maybe Plate
fromString input =
    let
        foundNumbers =
            Regex.find numbersRegex input |> List.map .match |> List.head

        foundLetters =
            Regex.find lettersRegex input |> List.map .match |> List.head
    in
        case ( foundLetters, foundNumbers ) of
            ( Just letters, Just numbers ) ->
                Just
                    { letters = letters
                    , numbers = numbers
                    , isEven = endsWithEven numbers
                    }

            ( _, Just numbers ) ->
                Just
                    { letters = ""
                    , numbers = numbers
                    , isEven = endsWithEven numbers
                    }

            _ ->
                Nothing


print : Plate -> String
print placa =
    (String.pad 3 'X' placa.letters) ++ "-" ++ placa.numbers |> String.toUpper
