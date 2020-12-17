module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Browser
import Css
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http
import Json.Decode as D exposing (string)
import Json.Encode as E exposing (string)
import Task



-- MAIN


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> ClickedLink
        , onUrlChange = \_ -> UrlChange
        }



-- MODEL


type alias Model =
    {}


init : Flags -> url -> key -> ( Model, Cmd Msg )
init _ _ _ =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = UrlChange
    | ClickedLink


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- Commands
-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        body : List (Html msg)
        body =
            [ div
                [ css
                    [ Css.height <| Css.pct 100
                    , Css.width <| Css.pct 100
                    , Css.backgroundColor <| Css.hex "000000"
                    , Css.color <| Css.hex "ffffff"
                    ]
                ]
                [ h1 [ css [ Css.textAlign Css.center ] ] [ text "Home" ]
                , Grid.row []
                    [ Grid.col []
                        [ Card.config [ Card.outlinePrimary ]
                            |> Card.headerH4 [] [ text "Getting started" ]
                            |> Card.block []
                                [ Block.text [] [ text "Getting started is real easy. Just click the start button." ]
                                , Block.custom <|
                                    Button.linkButton
                                        [ Button.primary ]
                                        [ text "Start" ]
                                ]
                            |> Card.view
                        ]
                    , Grid.col []
                        [ Card.config [ Card.outlineDanger ]
                            |> Card.headerH4 [] [ text "Modules" ]
                            |> Card.block []
                                [ Block.text [] [ text "Check out the modules overview" ]
                                , Block.custom <|
                                    Button.linkButton
                                        [ Button.primary ]
                                        [ text "Module" ]
                                ]
                            |> Card.view
                        ]
                    ]
                ]
            ]
    in
    { title = "Elm Bootstrap"
    , body = body
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
