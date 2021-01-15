module Data.Template exposing (Template, decode, decoder, toString)

import Data.Refinement as Refinement exposing (IntExp)
import Parser exposing ((|.), (|=), Parser, Problem(..), Trailing(..))


type alias Template =
    ( Int, List ( String, IntExp ) )


toString : Template -> String
toString ( int, list ) =
    "[kappa_"
        ++ String.fromInt int
        ++ "]_{"
        ++ (list
                |> List.map (\( k, v ) -> "(" ++ k ++ "," ++ (v |> Refinement.intExpToString) ++ ")")
                |> String.join ","
           )
        ++ "}"


decoder : Parser Template
decoder =
    let
        item : Parser ( String, IntExp )
        item =
            Parser.succeed (\a b -> ( a, b ))
                |. Parser.keyword "("
                |= Refinement.variableDecoder
                |. Parser.keyword ","
                |= Refinement.intExpDecoder
                |. Parser.keyword ")"
    in
    Parser.succeed (\a b -> ( a, b ))
        |. Parser.symbol "[k"
        |= Parser.int
        |= Parser.sequence
            { start = "]_{"
            , separator = ","
            , end = "}"
            , spaces = Parser.spaces
            , item = item
            , trailing = Forbidden
            }


decode : String -> Result String Template
decode =
    Parser.run
        decoder
        >> Result.mapError Refinement.deadEndsToString
