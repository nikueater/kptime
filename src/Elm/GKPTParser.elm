module GKPTParser exposing (..)

import GKPTEmoji as Emoji
import GKPTModel exposing (..)
import Parser exposing (..)


type alias ParseResult =
    Result String (List Segment)


parse : String -> ParseResult
parse code =
    case run segments code of
        Err e ->
            Err (deadEndsToString e)

        Ok s ->
            Ok s


text : Parser String
text =
    succeed Emoji.convert
        |= (getChompedString <|
                chompWhile (\x -> x /= '[' && x /= ']' && x /= '\n' && x /= '\u{000D}')
           )


relation : Parser Relation
relation =
    oneOf
        [ succeed (\x -> Relation x)
            |. backtrackable (symbol "[")
            |= text
            |. spaces
            |. symbol "]"
        , succeed None
        ]


segmentName : Parser String
segmentName =
    succeed String.trim
        |. symbol "#"
        |. spaces
        |= text
        |. chompUntilEndOr "\n"


node : Parser Node
node =
    succeed (\x y -> Node { text = String.trim x, relation = y })
        |. spaces
        |. symbol "-"
        |= text
        |= relation
        |. chompUntilEndOr "\n"


nodes : Parser (List Node)
nodes =
    let
        helper : List Node -> Parser (Step (List Node) (List Node))
        helper ns =
            oneOf
                [ succeed (\n -> Loop (n :: ns))
                    |= backtrackable node
                , succeed (Done (List.reverse ns))
                ]
    in
    loop [] helper


segment : Parser Segment
segment =
    succeed Segment
        |= segmentName
        |= nodes


segments : Parser (List Segment)
segments =
    let
        helper : List Segment -> Parser (Step (List Segment) (List Segment))
        helper segs =
            oneOf
                [ succeed (\seg -> Loop (seg :: segs))
                    |. backtrackable spaces
                    |= segment
                , succeed (Done (List.reverse segs))
                    |. spaces
                ]
    in
    succeed identity
        |= loop [] helper
        |. end
