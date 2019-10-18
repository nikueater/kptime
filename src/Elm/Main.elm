module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (labelHidden, multiline)
import GKPTModel exposing (..)
import GKPTParser as Parser


default : String
default =
    """
# Good
- something good :heart:

# Keep
- our culture

# Problem
- a problem [1]

# Try
- what you will do
- for instance, jogging [1]
"""


type alias Model =
    { segments : List Segment
    , code : String
    , error : String
    }


type Msg
    = OnEdit String


initialModel : Model
initialModel =
    parse default <|
        { segments = []
        , code = ""
        , error = ""
        }


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnEdit code ->
            ( parse code model, Cmd.none )


parse : String -> Model -> Model
parse code model =
    case Parser.parse code of
        Ok segs ->
            { model | code = code, segments = segs, error = "" }

        Err e ->
            { model | code = code, error = e }


view : Model -> Browser.Document Msg
view model =
    { title = "kptime"
    , body =
        [ layout
            [ width fill
            , height fill
            , clipY
            , Bg.color (rgb255 33 33 33)
            , inFront github
            ]
            (row [ width fill, height fill ]
                [ column
                    [ width (fillPortion 2)
                    , height fill
                    , spacing 0
                    , Bg.color (rgb255 51 51 51)
                    ]
                    [ row
                        [ width fill
                        , Font.color (rgb255 128 128 128)
                        , paddingEach
                            { top = 4
                            , left = 4
                            , right = 4
                            , bottom = 0
                            }
                        ]
                        [ el
                            [ Font.size 64
                            , Font.extraBold
                            , alignBottom
                            ]
                            (text "K")
                        , el
                            [ Font.size 48
                            , Font.heavy
                            , alignBottom
                            , paddingXY 0 2
                            ]
                            (text "PTIME")
                        ]
                    , el
                        [ paddingXY 8 0
                        , Font.color (rgb255 51 51 51)
                        , Font.heavy
                        , Font.size 18
                        , width fill
                        , Bg.color (rgb255 128 128 128)
                        ]
                        (text "for retrospectives")
                    , el
                        [ height fill
                        , width fill
                        , clipY
                        , scrollbarY
                        ]
                        (multiline
                            [ Bg.color (rgb255 51 51 51)
                            , Border.width 0
                            , Font.color (rgb255 255 255 255)
                            , Font.size 16
                            , Font.family [ Font.typeface "Fira Code" ]
                            , Element.focused [ Border.glow (Element.rgba 0 0 0 0) 2 ]
                            , height fill
                            ]
                            { onChange = OnEdit
                            , text = model.code
                            , placeholder = Nothing
                            , label = labelHidden "code"
                            , spellcheck = False
                            }
                        )
                    ]
                , column
                    [ width (fillPortion 7)
                    , height fill
                    , clipX
                    , scrollbarX
                    ]
                    [ segments model.segments
                    , column
                        [ width fill
                        , height (px 48)
                        ]
                        [ errorMessage model.error
                        ]
                    ]
                ]
            )
        ]
    }


segments : List Segment -> Element Msg
segments ss =
    row
        [ spacing 8
        , height fill
        , width fill
        , paddingXY 8 4
        , clipY
        ]
    <|
        List.map segment ss


segment : Segment -> Element Msg
segment (Segment name nodes) =
    column
        [ width (minimum 240 fill)
        , height fill
        , spacing 4
        ]
        [ wrappedRow
            [ Font.color (rgb255 255 255 255)
            , Font.bold
            , width fill
            , spacing 2
            ]
            [ el
                [ Font.size 24
                ]
                (paragraph [ width shrink ] [ text name ])
            , el
                [ Font.size 16
                , alignBottom
                , width fill
                ]
                (text <| String.fromInt (List.length nodes))
            ]
        , column
            [ spacing 6
            , width fill
            , height fill
            , scrollbarY
            ]
            (List.map node (List.reverse nodes))
        ]


node : Node -> Element Msg
node (Node n) =
    let
        relation =
            case n.relation of
                None ->
                    none

                Relation r ->
                    el
                        [ width fill
                        , height shrink
                        , Font.size 14
                        , Font.heavy
                        , paddingXY 4 2
                        , Font.color (rgb255 51 51 51)
                        , Bg.color (rgb255 178 80 0)
                        ]
                        (text r)
    in
    column
        [ width fill
        , Bg.color (rgb255 51 51 51)
        , Border.rounded 3
        , clip
        , alignBottom
        , Font.family [ Font.typeface "Fira Code" ]
        ]
        [ relation
        , paragraph
            [ padding 6
            , width fill
            , height (minimum 48 shrink)
            , Font.size 16
            , Font.color (rgb255 255 255 255)
            ]
            [ text n.text ]
        ]


errorMessage : String -> Element Msg
errorMessage msg =
    case msg of
        "" ->
            none

        _ ->
            el
                [ width fill
                , Bg.color (rgb255 180 0 0)
                ]
                (text msg)


github : Element Msg
github =
    newTabLink [ alignRight ]
        { url = "https://github.com/nikueater/kptime"
        , label =
            image
                [ width (px 120)
                , height (px 120)
                , alignRight
                ]
                { src = "https://github.blog/wp-content/uploads/2008/12/forkme_right_orange_ff7600.png?resize=298%2C298"
                , description = "fork me on github"
                }
        }
