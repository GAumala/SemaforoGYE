module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Time
import Task
import Url exposing (Url)
import Url.Parser
import Util exposing (readSystemTime)
import Plate exposing (Plate)
import Router exposing (Route(..), routeParser)
import Semaforo
import View


---- MODEL ----


type alias SearchModel =
    { input : String, badInput : Bool }


type alias ResultModel =
    { plate : Plate, date : Time.Posix, zone : Time.Zone, isAllowed : Bool }


type Page
    = SearchPage SearchModel
    | ResultPage ResultModel
    | NotFoundPage


defaultPage =
    SearchPage
        { input = ""
        , badInput = False
        }


type alias Model =
    { key : Nav.Key, page : Page, relativePath : String }


init : String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    updateWithUrl url { key = key, relativePath = flags, page = defaultPage }



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | SetPlate String
    | SubmitPlate
    | ValidatePlate Plate ( Time.Zone, Time.Posix )


updateWithUrl : Url -> Model -> ( Model, Cmd Msg )
updateWithUrl url model =
    case ( Url.Parser.parse routeParser url, model.page ) of
        ( Just Home, SearchPage _ ) ->
            ( model, Cmd.none )

        ( Just Home, ResultPage pageModel ) ->
            let
                newPage =
                    SearchPage
                        { input = Plate.print pageModel.plate
                        , badInput = False
                        }
            in
                ( { model | page = newPage }, Cmd.none )

        ( Just Home, _ ) ->
            ( { model | page = defaultPage }, Cmd.none )

        ( Just (QueryPlate str), _ ) ->
            case Plate.fromString str of
                Just plate ->
                    ( model, Task.perform (ValidatePlate plate) readSystemTime )

                Nothing ->
                    let
                        newPage =
                            SearchPage { input = str, badInput = True }
                    in
                        ( { model | page = newPage }, Cmd.none )

        _ ->
            ( { model | page = NotFoundPage }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            updateWithUrl url model

        ( SetPlate placa, SearchPage pageModel ) ->
            let
                newPage =
                    SearchPage
                        { pageModel
                            | input = placa
                            , badInput = False
                        }
            in
                ( { model | page = newPage }, Cmd.none )

        ( SubmitPlate, SearchPage pageModel ) ->
            case Plate.fromString pageModel.input of
                Just plate ->
                    let
                        queryString =
                            "?consulta=" ++ Plate.print plate

                        newUrl =
                            model.relativePath ++ queryString
                    in
                        ( model, Nav.pushUrl model.key newUrl )

                Nothing ->
                    ( { model | page = SearchPage { pageModel | badInput = True } }
                    , Cmd.none
                    )

        ( ValidatePlate plate ( zone, time ), _ ) ->
            let
                newPage =
                    ResultPage
                        { plate = plate
                        , date = time
                        , zone = zone
                        , isAllowed = Semaforo.isAllowed plate zone time
                        }
            in
                ( { model | page = newPage }, Cmd.none )

        -- invalid combinations
        ( SetPlate placa, _ ) ->
            ( model, Cmd.none )

        ( SubmitPlate, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


bodyView : Model -> Html Msg
bodyView m =
    case m.page of
        SearchPage model ->
            View.searchPage
                { plate = model.input
                , badInput = model.badInput
                , onPlateInput = SetPlate
                , onSubmitClick = SubmitPlate
                , relativePath = m.relativePath
                }

        ResultPage model ->
            View.resultPage
                { plate = model.plate
                , zone = model.zone
                , date = model.date
                , isAllowed = model.isAllowed
                , relativePath = m.relativePath
                }

        _ ->
            View.notFoundPage { relativePath = m.relativePath }


view : Model -> Browser.Document Msg
view model =
    { title = "Semáforo GYE"
    , body = [ bodyView model ]
    }



---- PROGRAM ----


main : Program String Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
