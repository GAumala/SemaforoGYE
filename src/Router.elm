module Router exposing (Route(..), routeParser)

import Url.Parser exposing (Parser, (<?>), top, map, oneOf)
import Url.Parser.Query as Query


type Route
    = Home
    | QueryPlate String


maybeQueryPlate maybeQueryParam =
    case maybeQueryParam of
        Just query ->
            QueryPlate query

        Nothing ->
            Home


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map maybeQueryPlate
            (top <?> Query.string "consulta")
        , map Home top
        ]
