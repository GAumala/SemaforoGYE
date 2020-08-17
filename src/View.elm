module View exposing (notFoundPage, searchPage, resultPage)

import Html
    exposing
        ( Html
        , div
        , input
        , img
        , a
        , form
        , button
        , text
        , p
        , section
        , h1
        , h2
        , footer
        )
import Html.Attributes
    exposing
        ( class
        , type_
        , placeholder
        , value
        , href
        , src
        , height
        , width
        )
import Html.Events exposing (onClick, onInput, onSubmit)
import Plate
import Util exposing (printReadableDate)


badInputMessage badInput =
    if badInput then
        "Por favor ingresa una placa válida"
    else
        ""


searchPage { plate, onPlateInput, onSubmitClick, badInput } =
    form [ class "regular-page", onSubmit onSubmitClick ]
        [ img [ src "/emoji.png", height 128, width 128 ] []
        , p [ class "subtitle" ]
            [ text "¿Puede tu vehículo circular hoy en GYE?" ]
        , div
            [ class "field has-addons has-addons-centered" ]
            [ div [ class "control" ]
                [ input
                    [ class "input"
                    , type_ "text"
                    , placeholder "Ingresa tu placa"
                    , value plate
                    , onInput onPlateInput
                    ]
                    []
                ]
            , div [ class "control" ]
                [ input
                    [ class "button is-info"
                    , type_ "submit"
                    , onClick onSubmitClick
                    , value "Consultar"
                    ]
                    []
                ]
            ]
        , p [ class "help is-danger" ]
            [ text <| badInputMessage badInput ]
        ]


newQueryFooter =
    footer [ class "footer" ]
        [ a [ href "/" ] [ text "Nueva consulta" ] ]


resultPage { plate, zone, date, isAllowed } =
    let
        sectionClass =
            if isAllowed then
                "hero is-success"
            else
                "hero is-danger"

        sectionTitle =
            if isAllowed then
                " SÍ puedes circular"
            else
                "NO puedes circular"

        yesOrNo =
            if isAllowed then
                ") sí "
            else
                ") no "

        plateString =
            Plate.print plate

        dateString =
            printReadableDate zone date

        sectionSubtitle =
            "Tu placa ("
                ++ plateString
                ++ yesOrNo
                ++ "puede circular en GYE el día de hoy, "
                ++ dateString
                ++ "."
    in
        div []
            [ section [ class sectionClass ]
                [ div [ class "hero-body" ]
                    [ div [ class "container" ]
                        [ h1 [ class "title" ] [ text sectionTitle ]
                        , h2 [ class "subtitle" ] [ text sectionSubtitle ]
                        ]
                    ]
                ]
            , newQueryFooter
            ]


notFoundPage =
    div [ class "regular-page" ]
        [ h1 [ class "title" ] [ text "Página No Encontrada" ]
        , newQueryFooter
        ]
