module Main exposing (main)

import Browser
import Colors
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Events
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


type alias Model =
    { segments : List Segment
    , code : String
    , error : String
    , relation : Relation
    }


type Msg
    = OnEdit String
    | OnEnterNode Relation
    | OnLeaveNode Relation


initialModel : Model
initialModel =
    parse default <|
        { segments = []
        , code = ""
        , error = ""
        , relation = None
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

        OnEnterNode rel ->
            ( { model | relation = rel }, Cmd.none )

        OnLeaveNode rel ->
            if model.relation == rel then
                ( { model | relation = None }, Cmd.none )

            else
                ( model, Cmd.none )


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
            , Bg.color Colors.white
            , inFront github
            ]
            (column
                [ width fill
                , height fill
                ]
                [ el
                    [ width fill
                    , Bg.color Colors.blue
                    , Font.color Colors.white
                    , Font.heavy
                    , Font.size 24
                    , padding 8
                    ]
                    (text "kptime")
                , row
                    [ width fill
                    , height fill
                    , clip
                    ]
                    [ el
                        [ width (fillPortion 2)
                        , height fill
                        , spacing 0
                        , clip
                        , scrollbarY
                        , Bg.color Colors.lightGrey
                        ]
                        (multiline
                            [ height fill
                            , Bg.color Colors.lightGrey
                            , Border.width 0
                            , Font.color Colors.darkGrey
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
                        , clip
                        , scrollbarX
                        ]
                        [ segments model.relation model.segments
                        , errorMessage model.error
                        ]
                    ]
                ]
            )
        ]
    }


segments : Relation -> List Segment -> Element Msg
segments relation ss =
    row
        [ spacing 8
        , height fill
        , width fill
        , paddingXY 8 4
        , clipY
        , scrollbarY
        ]
    <|
        List.map (segment relation) ss


segment : Relation -> Segment -> Element Msg
segment relation (Segment name nodes) =
    let
        color =
            Colors.colorOf name
    in
    column
        [ width (minimum 240 fill)
        , height fill
        , spacing 4
        ]
        [ wrappedRow
            [ Font.color Colors.white
            , Font.bold
            , width fill
            , spacing 2
            ]
            [ el
                [ Font.size 24
                , Font.color color
                ]
                (paragraph [ width shrink ] [ text name ])
            , el
                [ Font.size 16
                , Font.color Colors.darkGrey
                , alignBottom
                , width fill
                ]
                (el
                    [ width (px 24)
                    , height (px 24)
                    , Bg.color color
                    , Border.rounded 12
                    , alignBottom
                    ]
                    (el
                        [ centerX
                        , centerY
                        , Font.color Colors.white
                        ]
                        (text <| String.fromInt (List.length nodes))
                    )
                )
            ]
        , column
            [ spacing 6
            , width fill
            , height fill
            , scrollbarY
            ]
            (List.map (node relation color) (List.reverse nodes))
        ]


node : Relation -> Color -> Node -> Element Msg
node highlight color (Node n) =
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
                        , padding 5
                        , Font.center
                        , Font.color Colors.white
                        , Bg.color Colors.translucentBlack
                        , Border.dashed
                        , Border.color Colors.translucentWhite
                        , Border.widthEach
                            { top = 0
                            , left = 0
                            , right = 0
                            , bottom = 1
                            }
                        ]
                        (text r)

        events =
            case n.relation of
                None ->
                    []

                r ->
                    [ Events.onMouseEnter (OnEnterNode r)
                    , Events.onMouseLeave (OnLeaveNode r)
                    ]

        opacity =
            if highlight /= None && highlight /= n.relation then
                0.25

            else
                1.0
    in
    column
        ([ width fill
         , Bg.color color
         , Border.rounded 3
         , clip
         , alignBottom
         , height shrink
         , Font.family [ Font.typeface "Fira Code" ]
         , alpha opacity
         ]
            ++ events
        )
        [ relation
        , paragraph
            [ padding 6
            , width fill
            , height fill
            , clipX
            , Font.size 16
            , Font.color Colors.white
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
            el [ height (px 24) ]
                none

        _ ->
            el
                [ width fill
                , height (px 24)
                , centerY
                , Font.size 16
                , Font.heavy
                , Font.color Colors.orange
                ]
                (el [ centerX, centerY ] (text msg))


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
