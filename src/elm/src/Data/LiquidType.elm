module Data.LiquidType exposing (Input(..), IntExp(..), LiquidType, Refinement(..), SimpleLiquidType(..), decode, decodeRefinement, decodeSimpleLiquidType, toString)

import Dict exposing (Dict)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import Result.Extra as Result
import Set


type IntExp
    = Integer Int
    | Plus IntExp IntExp
    | Times IntExp Int
    | Var String


intExpDecoder : Parser IntExp
intExpDecoder =
    Parser.oneOf
        [ Parser.map Integer Parser.int
        , Parser.succeed Plus
            |. Parser.keyword "(+)"
            |. Parser.spaces
            |= Parser.lazy (\() -> intExpDecoder)
            |. Parser.spaces
            |= Parser.lazy (\() -> intExpDecoder)
        , Parser.succeed Times
            |. Parser.keyword "(*)"
            |. Parser.spaces
            |= Parser.lazy (\() -> intExpDecoder)
            |. Parser.spaces
            |= Parser.int
        , Parser.map Var
            (Parser.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.fromList [ "or", "and", "not" ]
                }
            )
        ]


type Refinement
    = IsTrue
    | IsFalse
    | IsSmaller String IntExp
    | IsBigger String IntExp
    | IsEqual String IntExp
    | EitherOr Refinement Refinement
    | AndAlso Refinement Refinement
    | IsNot Refinement


refinementDecoder : Parser Refinement
refinementDecoder =
    Parser.oneOf
        [ Parser.map (\_ -> IsTrue) (Parser.keyword "True")
        , Parser.map (\_ -> IsFalse) (Parser.keyword "False")
        , Parser.succeed IsSmaller
            |. Parser.keyword "(<)"
            |. Parser.spaces
            |= Parser.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.fromList [ "or", "and", "not" ]
                }
            |. Parser.spaces
            |= intExpDecoder
        , Parser.succeed IsBigger
            |. Parser.keyword "(>)"
            |. Parser.spaces
            |= Parser.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.fromList [ "or", "and", "not" ]
                }
            |. Parser.spaces
            |= intExpDecoder
        , Parser.succeed IsEqual
            |. Parser.keyword "(==)"
            |. Parser.spaces
            |= Parser.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.fromList [ "or", "and", "not" ]
                }
            |. Parser.spaces
            |= intExpDecoder
        , Parser.succeed EitherOr
            |. Parser.keyword "or"
            |. Parser.spaces
            |= Parser.lazy (\() -> refinementDecoder)
            |. Parser.spaces
            |= Parser.lazy (\() -> refinementDecoder)
        , Parser.succeed AndAlso
            |. Parser.keyword "and"
            |. Parser.spaces
            |= Parser.lazy (\() -> refinementDecoder)
            |. Parser.spaces
            |= Parser.lazy (\() -> refinementDecoder)
        , Parser.succeed IsNot
            |. Parser.keyword "not"
            |. Parser.spaces
            |= Parser.lazy (\() -> refinementDecoder)
        ]


decodeRefinement : String -> Result String Refinement
decodeRefinement =
    Parser.run refinementDecoder
        >> Result.mapError Parser.deadEndsToString


type SimpleLiquidType
    = IntType Refinement
    | Template Int


type alias LiquidType =
    ( SimpleLiquidType, List SimpleLiquidType )


type Input
    = IntegerInput Int
    | StringInput String


decodeSimpleLiquidType : Input -> Result String SimpleLiquidType
decodeSimpleLiquidType input =
    case input of
        IntegerInput n ->
            Ok <| Template n

        StringInput string ->
            string
                |> decodeRefinement
                |> Result.map IntType


decode : ( Input, List Input ) -> Result String LiquidType
decode ( head, list ) =
    Result.map2
        (\a b -> ( a, b ))
        (head |> decodeSimpleLiquidType)
        (list
            |> List.map decodeSimpleLiquidType
            |> Result.combine
        )


intExpToString : IntExp -> String
intExpToString input =
    case input of
        Integer int ->
            String.fromInt int

        Plus intExp1 intExp2 ->
            "(+) " ++ intExpToString intExp1 ++ " " ++ intExpToString intExp2

        Times intExp i ->
            "(*) " ++ intExpToString intExp ++ " " ++ String.fromInt i

        Var string ->
            string


refinementToString : Refinement -> String
refinementToString refinement =
    case refinement of
        IsTrue ->
            "True"

        IsFalse ->
            "False"

        IsSmaller string intExp ->
            "(<) " ++ string ++ " " ++ intExpToString intExp

        IsBigger string intExp ->
            "(>) " ++ string ++ " " ++ intExpToString intExp

        IsEqual string intExp ->
            "(==) " ++ string ++ " " ++ intExpToString intExp

        EitherOr r1 r2 ->
            "Or " ++ refinementToString r1 ++ " " ++ refinementToString r2

        AndAlso r1 r2 ->
            "And " ++ refinementToString r1 ++ " " ++ refinementToString r2

        IsNot r ->
            "Not " ++ refinementToString r


simpleLiquidTypeToString : SimpleLiquidType -> String
simpleLiquidTypeToString simpleLiquidType =
    case simpleLiquidType of
        IntType refinement ->
            "{nu:Int |" ++ (refinement |> refinementToString) ++ "}"

        Template int ->
            "{nu:Int | kappa_" ++ String.fromInt int ++ "}"


toString : LiquidType -> String
toString ( head, tail ) =
    head
        :: tail
        |> List.map simpleLiquidTypeToString
        |> String.join " -> "
