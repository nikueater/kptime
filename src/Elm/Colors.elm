module Colors exposing (..)

import Element exposing (Color, rgb255, rgba255)


colorOf : String -> Color
colorOf s =
    case String.toLower s of
        "good" ->
            blue

        "keep" ->
            lightGreen

        "problem" ->
            deepPurple

        "try" ->
            deepOrange

        _ ->
            darkGrey


white : Color
white =
    rgb255 236 239 241


darkGrey : Color
darkGrey =
    rgb255 55 71 79


grey : Color
grey =
    rgb255 51 51 51


lightGrey : Color
lightGrey =
    rgb255 207 216 220


orange : Color
orange =
    rgb255 178 80 0


blue : Color
blue =
    rgb255 33 150 243


lightGreen : Color
lightGreen =
    rgb255 139 195 74


deepOrange : Color
deepOrange =
    rgb255 255 87 34


deepPurple : Color
deepPurple =
    rgb255 103 58 183


translucentBlack : Color
translucentBlack =
    rgba255 0 0 0 0.3


translucentWhite : Color
translucentWhite =
    rgba255 255 255 255 0.5
