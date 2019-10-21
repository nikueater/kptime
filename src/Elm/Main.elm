module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (labelHidden, multiline)
import GKPTModel exposing (..)
import GKPTParser as Parser
import Html.Attributes exposing (style)


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


white : Color
white =
    rgb255 255 255 255


darkGrey : Color
darkGrey =
    rgb255 33 33 33


grey : Color
grey =
    rgb255 51 51 51


lightGrey : Color
lightGrey =
    rgb255 128 128 128


orange : Color
orange =
    rgb255 178 80 0


blue : Color
blue =
    rgb255 33 150 243


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
            , Bg.color darkGrey
            , inFront github
            ]
            (row [ width fill, height fill ]
                [ el
                    [ width (fillPortion 2)
                    , height fill
                    , spacing 0
                    , clip
                    , scrollbarY
                    , Bg.color grey
                    ]
                    (multiline
                        [ height fill
                        , Bg.color grey
                        , Border.width 0
                        , Font.color white
                        , Font.size 16
                        , Font.family [ Font.typeface "Fira Code" ]
                        , Element.focused [ Border.glow (Element.rgba 0 0 0 0) 2 ]
                        ]
                        { onChange = OnEdit
                        , text = model.code
                        , placeholder = Nothing
                        , label = labelHidden "code"
                        , spellcheck = False
                        }
                    )
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
            [ Font.color white
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
                        , Font.color grey
                        , Bg.color orange
                        ]
                        (text r)
    in
    column
        [ width fill
        , Bg.color grey
        , Border.rounded 3
        , clip
        , alignBottom
        , height shrink
        , Font.family [ Font.typeface "Fira Code" ]
        ]
        [ relation
        , paragraph
            [ padding 6
            , width fill
            , height fill
            , clipX
            , Font.size 16
            , Font.color white
            , htmlAttribute (style "white-space" "normal")
            , htmlAttribute (style "word-wrap" "break-word")
            ]
            [ el
                []
                (text n.text)
            ]
        ]


errorMessage : String -> Element Msg
errorMessage msg =
    case msg of
        "" ->
            none

        _ ->
            el
                [ width fill
                , Bg.color orange
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
