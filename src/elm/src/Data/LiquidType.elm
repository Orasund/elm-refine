module Data.LiquidType exposing (Input(..), LiquidType, LiquidTypeForm, SimpleLiquidType(..), WellFormedLiquidType, decodeSimpleLiquidType, formToString, simpleLiquidTypeToString, simpleformToString, toString)

import Array exposing (Array)
import Data.Refinement as Refinement exposing (IntExp, Refinement(..))
import Data.Template as Template exposing (Template)
import Dict exposing (Dict)
import List.Extra as List
import Parser exposing ((|.), (|=), DeadEnd, Parser, Problem(..), Trailing(..))
import Result.Extra as Result


type SimpleLiquidType
    = IntType Refinement
    | LiquidTypeVariable Template


type alias LiquidType a b =
    ( List { name : String, baseType : a }, b )


type alias WellFormedLiquidType =
    LiquidType Refinement Template


type alias LiquidTypeForm =
    ( Array { name : String, baseType : String }, String )


type Input
    = IntegerInput Int
    | StringInput String


substituteTemplate : Dict Int Refinement -> SimpleLiquidType -> Refinement
substituteTemplate dict liquidType =
    case liquidType of
        IntType refinement ->
            refinement

        LiquidTypeVariable ( int, list ) ->
            list
                |> List.foldl
                    (\( find, replaceWith ) ->
                        Refinement.substitute { find = find, replaceWith = replaceWith }
                    )
                    (dict
                        |> Dict.get int
                        |> Maybe.withDefault IsFalse
                    )


decodeSimpleLiquidType : String -> Result String SimpleLiquidType
decodeSimpleLiquidType =
    Parser.run
        (Parser.oneOf
            [ Refinement.decoder |> Parser.map IntType
            , Template.decoder |> Parser.map LiquidTypeVariable
            ]
        )
        >> Result.mapError Refinement.deadEndsToString


simpleformToString : String -> String
simpleformToString var =
    "{v:Int|"
        ++ var
        ++ "}"


formToString : String -> String -> LiquidTypeForm -> String
formToString name typeVar form =
    (form
        |> Tuple.first
        |> Array.indexedMap
            (\i _ ->
                name
                    ++ String.fromInt (i + 1)
                    ++ " : "
                    ++ simpleformToString
                        (typeVar
                            ++ String.fromInt
                                ((form
                                    |> Tuple.first
                                    |> Array.length
                                 )
                                    - i
                                )
                        )
                    ++ " -> "
            )
        |> Array.toList
        |> String.concat
    )
        ++ simpleformToString (typeVar ++ String.fromInt 0)


simpleLiquidTypeToString : SimpleLiquidType -> String
simpleLiquidTypeToString simpleLiquidType =
    case simpleLiquidType of
        IntType refinement ->
            refinement |> Refinement.toString

        LiquidTypeVariable template ->
            template |> Template.toString


toString : (a -> String) -> (b -> String) -> LiquidType a b -> String
toString aToString bToString ( list, last ) =
    ((list
        |> List.indexedMap
            (\i { name, baseType } ->
                name
                    ++ " : "
                    ++ simpleformToString (baseType |> aToString)
            )
     )
        ++ [ simpleformToString (last |> bToString) ]
    )
        |> String.join " -> "
