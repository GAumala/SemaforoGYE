module Router exposing (Route(..), routeParser)

import Url.Parser exposing (Parser, (<?>), top, map, oneOf, s)
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

        -- this one is only for gh pages
        , map maybeQueryPlate
            (s "SemaforoGYE" <?> Query.string "consulta")
        , map Home top
        ]
