module Example exposing (..)

import GKPTModel exposing (..)
import GKPTParser as KPT exposing (..)
import Test exposing (..)
import Utils exposing (check)


list : Test
list =
    describe "list"
        [ test "simply parse a list item" <|
            \_ ->
                "- helloworld"
                    |> check KPT.node
                        (Node
                            { text = "helloworld"
                            , relation = None
                            }
                        )
        ]


name : Test
name =
    describe "segment name"
        [ test "get name" <|
            \_ ->
                "#Good"
                    |> check KPT.segmentName "Good"
        ]


lists : Test
lists =
    describe "lists"
        [ test "multiple list items at the top level" <|
            \_ ->
                "- hello world\n- foo bar"
                    |> check KPT.nodes
                        [ Node
                            { text = "hello world"
                            , relation = None
                            }
                        , Node
                            { text = "foo bar"
                            , relation = None
                            }
                        ]
        ]


segment : Test
segment =
    describe "segment"
        [ test "get segment" <|
            \_ ->
                "#Good \n - hello world\n- foo bar\n"
                    |> check KPT.segment
                        (Segment "Good"
                            [ Node
                                { text = "hello world"
                                , relation = None
                                }
                            , Node
                                { text = "foo bar"
                                , relation = None
                                }
                            ]
                        )
        ]


segments : Test
segments =
    describe "segment list"
        [ test "get segments" <|
            \_ ->
                "#Good \n - hello world\n- foo bar\n#Keep \n - keep nothing"
                    |> check KPT.segments
                        [ Segment "Good"
                            [ Node
                                { text = "hello world"
                                , relation = None
                                }
                            , Node
                                { text = "foo bar"
                                , relation = None
                                }
                            ]
                        , Segment "Keep"
                            [ Node
                                { text = "keep nothing"
                                , relation = None
                                }
                            ]
                        ]
        ]
