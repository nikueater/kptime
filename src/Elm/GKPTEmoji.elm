module GKPTEmoji exposing (convert)


convert : String -> String
convert s =
    List.foldl (\( emoji, name ) cs -> String.replace (":" ++ name ++ ":") emoji cs) s emojis


emojis : List ( String, String )
emojis =
    [ ( "👍", "+1" )
    , ( "❤️", "heart" )
    , ( "🔥", "fire" )
    , ( "\u{1F914}", "thinking" )
    ]
