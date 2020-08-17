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
    { key : Nav.Key, page : Page, rootPath : String }


init : String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    updateWithUrl url { key = key, rootPath = flags, page = defaultPage }



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | InputPlate String
    | SubmitPlate
    | ShowResult Plate ( Time.Zone, Time.Posix )


updateWithUrl : Url -> Model -> ( Model, Cmd Msg )
updateWithUrl url model =
    case ( Url.Parser.parse routeParser url, model.page ) of
        ( Just Home, SearchPage _ ) ->
            ( model, Cmd.none )

        ( Just Home, _ ) ->
            ( { model | page = defaultPage }, Cmd.none )

        ( Just (QueryPlate str), _ ) ->
            case Plate.fromString str of
                Just plate ->
                    ( model, Task.perform (ShowResult plate) readSystemTime )

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

        ( InputPlate placa, SearchPage pageModel ) ->
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
                            model.rootPath ++ queryString
                    in
                        ( model, Nav.pushUrl model.key newUrl )

                Nothing ->
                    ( { model | page = SearchPage { pageModel | badInput = True } }
                    , Cmd.none
                    )

        ( ShowResult plate ( zone, time ), _ ) ->
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
        ( InputPlate _, _ ) ->
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
                , onPlateInput = InputPlate
                , onSubmitClick = SubmitPlate
                }

        ResultPage model ->
            View.resultPage
                { plate = model.plate
                , zone = model.zone
                , date = model.date
                , isAllowed = model.isAllowed
                , rootPath = m.rootPath
                }

        _ ->
            View.notFoundPage { rootPath = m.rootPath }


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
