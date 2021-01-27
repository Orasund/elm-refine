module Data.Template exposing (Template, decode, decoder, toString)

import Data.IntExp as IntExp exposing (IntExp)
import Data.Refinement as Refinement
import Parser exposing ((|.), (|=), Parser, Problem(..), Trailing(..))


type alias Template =
    ( Int, List ( String, IntExp ) )


toString : Template -> String
toString ( int, list ) =
    "[kappa_"
        ++ String.fromInt int
        ++ "]_{"
        ++ (list
                |> List.map (\( k, v ) -> "(" ++ k ++ "," ++ (v |> IntExp.toString) ++ ")")
                |> String.join ","
           )
        ++ "}"


decoder : Parser Template
decoder =
    let
        item : Parser ( String, IntExp )
        item =
            Parser.succeed (\a b -> ( a, b ))
                |. Parser.symbol "("
                |= IntExp.variableDecoder
                |. Parser.symbol ","
                |= IntExp.decoder
                |. Parser.symbol ")"
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
