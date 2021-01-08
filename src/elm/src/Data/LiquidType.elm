module Data.LiquidType exposing (Input(..), IntExp(..), LiquidType, LiquidTypeForm, Refinement(..), SimpleLiquidType(..), decode, decodeRefinement, decodeSimpleLiquidType, formToString, toString)

import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra as List
import Parser exposing ((|.), (|=), DeadEnd, Parser, Problem(..))
import Result.Extra as Result
import Set


type IntExp
    = Integer Int
    | Plus IntExp IntExp
    | Times IntExp Int
    | Var String


variableDecoder : Parser String
variableDecoder =
    Parser.variable
        { start = Char.isLower
        , inner = Char.isAlphaNum
        , reserved = Set.fromList [ "or", "and", "not" ]
        }


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
        , Parser.map Var variableDecoder
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


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    String.concat
        (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Problem -> String
problemToString p =
    case p of
        Expecting s ->
            "expecting '" ++ s ++ "'"

        ExpectingInt ->
            "expecting int"

        ExpectingHex ->
            "expecting hex"

        ExpectingOctal ->
            "expecting octal"

        ExpectingBinary ->
            "expecting binary"

        ExpectingFloat ->
            "expecting float"

        ExpectingNumber ->
            "expecting number"

        ExpectingVariable ->
            "expecting variable"

        ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        ExpectingEnd ->
            "expecting end"

        UnexpectedChar ->
            "unexpected char"

        Problem s ->
            "problem " ++ s

        BadRepeat ->
            "bad repeat"


decodeRefinement : String -> Result String Refinement
decodeRefinement =
    Parser.run refinementDecoder
        >> Result.mapError deadEndsToString


type SimpleLiquidType
    = IntType Refinement
    | Template Int


type alias LiquidType =
    { name : String, baseType : ( List SimpleLiquidType, SimpleLiquidType ) }


type alias LiquidTypeForm =
    { name : String, baseType : ( Array String, String ) }


type Input
    = IntegerInput Int
    | StringInput String


decodeSimpleLiquidType : String -> Result String SimpleLiquidType
decodeSimpleLiquidType input =
    case input |> String.toInt of
        Just n ->
            Ok <| Template n

        Nothing ->
            input
                |> decodeRefinement
                |> Result.map IntType


decode : LiquidTypeForm -> Result String LiquidType
decode { name, baseType } =
    let
        ( list, head ) =
            baseType
    in
    Result.map2
        (\a b -> { name = name, baseType = ( a, b ) })
        (list
            |> Array.toList
            |> List.map decodeSimpleLiquidType
            |> Result.combine
        )
        (head |> decodeSimpleLiquidType)


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
            "{v:Int |" ++ (refinement |> refinementToString) ++ "}"

        Template int ->
            "{v:Int | kappa_" ++ String.fromInt int ++ "}"


formToString : String -> LiquidTypeForm -> String
formToString typeVar { name, baseType } =
    (baseType
        |> Tuple.first
        |> Array.indexedMap
            (\i _ ->
                name
                    ++ String.fromInt (i + 1)
                    ++ " : {v:Int|"
                    ++ typeVar
                    ++ String.fromInt (i + 1)
                    ++ "} -> "
            )
        |> Array.toList
        |> String.concat
    )
        ++ "{v:Int|"
        ++ typeVar
        ++ String.fromInt 0
        ++ "}"


toString : LiquidType -> String
toString { name, baseType } =
    let
        ( list, last ) =
            baseType
    in
    ((list
        |> List.indexedMap
            (\i simpleLiquidType ->
                name
                    ++ String.fromInt (i + 1)
                    ++ " : "
                    ++ (simpleLiquidType |> simpleLiquidTypeToString)
            )
     )
        ++ [ last |> simpleLiquidTypeToString ]
    )
        |> String.join " -> "
