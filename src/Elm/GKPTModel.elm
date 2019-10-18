module GKPTModel exposing (..)


segmentName : Segment -> String
segmentName (Segment name _) =
    name


segmentItems : Segment -> List Node
segmentItems (Segment _ xs) =
    xs


type Segment
    = Segment String (List Node)


type Relation
    = Relation String
    | None


type Node
    = Node
        { text : String
        , relation : Relation
        }
