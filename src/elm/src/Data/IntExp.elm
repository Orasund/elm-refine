module Data.IntExp exposing (IntExp(..), decoder, substitute, toSMTStatement, toString, variableDecoder, variables)

import Parser exposing ((|.), (|=), Parser, Problem(..))
import Set exposing (Set)


type IntExp
    = Integer Int
    | Plus IntExp IntExp
    | Times IntExp Int
    | Var String


variables : IntExp -> Set String
variables intExp =
    case intExp of
        Integer _ ->
            Set.empty

        Plus i1 i2 ->
            Set.union (variables i1) (variables i2)

        Times i _ ->
            variables i

        Var string ->
            Set.singleton string


variableDecoder : Parser String
variableDecoder =
    Parser.variable
        { start = Char.isLower
        , inner = Char.isAlphaNum
        , reserved = Set.fromList [ "or", "and", "not" ]
        }


decoder : Parser IntExp
decoder =
    let
        intDecoder =
            Parser.oneOf
                [ Parser.int
                , Parser.succeed identity
                    |. Parser.symbol "-"
                    |= Parser.map ((*) -1) Parser.int
                ]
    in
    Parser.oneOf
        [ Parser.map Integer intDecoder
        , Parser.succeed Plus
            |. Parser.keyword "(+)"
            |. Parser.spaces
            |= Parser.lazy (\() -> decoder)
            |. Parser.spaces
            |= Parser.lazy (\() -> decoder)
        , Parser.succeed Times
            |. Parser.keyword "(*)"
            |. Parser.spaces
            |= Parser.lazy (\() -> decoder)
            |. Parser.spaces
            |= intDecoder
        , Parser.map Var variableDecoder
        , Parser.succeed identity
            |. Parser.symbol "("
            |= Parser.lazy (\() -> decoder)
            |. Parser.symbol ")"
        ]


toString : IntExp -> String
toString input =
    case input of
        Integer int ->
            String.fromInt int

        Plus intExp1 intExp2 ->
            "(+) " ++ toString intExp1 ++ " " ++ toString intExp2

        Times intExp i ->
            "(*) " ++ toString intExp ++ " " ++ String.fromInt i

        Var string ->
            string


substitute : { find : String, replaceWith : IntExp } -> IntExp -> IntExp
substitute { find, replaceWith } intExp =
    case intExp of
        Var string ->
            if string == find then
                replaceWith

            else
                Var string

        _ ->
            intExp


toSMTStatement : IntExp -> String
toSMTStatement input =
    case input of
        Integer int ->
            String.fromInt int

        Plus intExp1 intExp2 ->
            "(+ " ++ toString intExp1 ++ " " ++ toString intExp2 ++ ")"

        Times intExp i ->
            "(* " ++ toString intExp ++ " " ++ String.fromInt i ++ ")"

        Var string ->
            string
