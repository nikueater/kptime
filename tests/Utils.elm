module Utils exposing (check)

import Expect exposing (Expectation)
import Parser as P


check : P.Parser a -> a -> String -> Expectation
check parser d =
    P.run parser >> okEqual d


okEqual : a -> Result c a -> Expectation
okEqual r b =
    case b of
        Ok a ->
            Expect.equal r a

        Err _ ->
            Expect.fail "failed to parse"
