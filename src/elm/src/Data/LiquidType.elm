module Data.LiquidType exposing (Input(..), LiquidType, LiquidTypeForm, SimpleLiquidType(..), decode, decodeRefinement, decodeSimpleLiquidType, formToString, toString)

import Array exposing (Array)
import Data.Refinement as Refinement exposing (Refinement)
import List.Extra as List
import Parser exposing (DeadEnd, Problem(..))
import Result.Extra as Result


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
    Parser.run Refinement.decode
        >> Result.mapError deadEndsToString


type SimpleLiquidType
    = IntType Refinement
    | Template Int


type alias LiquidType =
    { name : String
    , baseType : ( List SimpleLiquidType, SimpleLiquidType )
    }


type alias LiquidTypeForm =
    { name : String
    , baseType : ( Array String, String )
    }


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


simpleLiquidTypeToString : SimpleLiquidType -> String
simpleLiquidTypeToString simpleLiquidType =
    case simpleLiquidType of
        IntType refinement ->
            "{v:Int |" ++ (refinement |> Refinement.toString) ++ "}"

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
