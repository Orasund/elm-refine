module Data.Condition exposing
    ( Condition
    , ConditionForm
    , SimpleCondition
    , addBigger
    , addGuard
    , addSmaller
    , addTypeVariable
    , decode
    , emptyForm
    , liquidTypeVariables
    , removeBigger
    , removeGuard
    , removeSmaller
    , removeTypeVariable
    , setBigger
    , setGuard
    , setSmaller
    , setTypeVariableName
    , setTypeVariables
    , setVariable
    , toSMTStatement
    )

import Array exposing (Array)
import Array.Extra as Array
import Data.IntExp exposing (IntExp(..))
import Data.LiquidType as LiquidType exposing (Input(..), LiquidType, LiquidTypeForm, SimpleLiquidType(..))
import Data.Refinement as Refinement exposing (Refinement(..))
import Data.Template as Template exposing (Template)
import Dict exposing (Dict)
import List.Extra as List
import Parser
import Result.Extra as Result
import Set


type alias Condition =
    { smaller : LiquidType Template SimpleLiquidType
    , bigger : LiquidType Refinement Template
    , guards : List Refinement
    , typeVariables : List ( String, Refinement )
    }


type alias SimpleCondition =
    { smaller : SimpleLiquidType
    , bigger : Template
    , guards : List Refinement
    , typeVariables : List ( String, Refinement )
    }


type alias ConditionForm =
    { smaller : LiquidTypeForm
    , bigger : LiquidTypeForm
    , guards : Array String
    , typeVariables : Array ( String, String )
    }


liquidTypeVariables : SimpleCondition -> List Int
liquidTypeVariables { smaller, bigger } =
    (bigger |> Tuple.first)
        :: (case smaller of
                IntType _ ->
                    []

                LiquidTypeVariable ( int, _ ) ->
                    [ int ]
           )


addSmaller : ConditionForm -> ConditionForm
addSmaller form =
    { form
        | smaller =
            form.smaller
                |> Tuple.mapFirst
                    (\l ->
                        Array.append
                            ([ { name = "a" ++ String.fromInt (l |> Array.length |> (+) 1)
                               , refinement = "[k" ++ String.fromInt (l |> Array.length) ++ "]_{}"
                               }
                             ]
                                |> Array.fromList
                            )
                            l
                    )
    }


removeSmaller : ConditionForm -> ConditionForm
removeSmaller form =
    { form
        | smaller =
            form.smaller
                |> Tuple.mapFirst
                    (\l ->
                        l
                            |> Array.removeAt 0
                     --(l |> Array.length |> (+) -1)
                    )
    }


addBigger : ConditionForm -> ConditionForm
addBigger form =
    { form
        | bigger =
            form.bigger
                |> Tuple.mapFirst
                    (\l ->
                        Array.append
                            ([ { name = "a" ++ String.fromInt (l |> Array.length |> (+) 1)
                               , refinement = "True"
                               }
                             ]
                                |> Array.fromList
                            )
                            l
                    )
    }


removeBigger : ConditionForm -> ConditionForm
removeBigger form =
    { form
        | bigger =
            form.bigger
                |> Tuple.mapFirst
                    (\l ->
                        l
                            |> Array.removeAt 0
                     --(l |> Array.length |> (+) -1)
                    )
    }


addGuard : ConditionForm -> ConditionForm
addGuard form =
    { form
        | guards =
            Array.append form.guards ([ "False" ] |> Array.fromList)
    }


removeGuard : ConditionForm -> ConditionForm
removeGuard form =
    { form
        | guards =
            form.guards
                |> Array.removeAt (form.guards |> Array.length |> (+) -1)
    }


addTypeVariable : ConditionForm -> ConditionForm
addTypeVariable form =
    { form
        | typeVariables =
            Array.append form.typeVariables
                ([ ( "a" ++ (form.typeVariables |> Array.length |> String.fromInt)
                   , "True"
                   )
                 ]
                    |> Array.fromList
                )
    }


removeTypeVariable : ConditionForm -> ConditionForm
removeTypeVariable form =
    { form
        | typeVariables =
            form.typeVariables
                |> Array.removeAt (form.typeVariables |> Array.length |> (+) -1)
    }


setSmaller : Int -> String -> ConditionForm -> ConditionForm
setSmaller index value form =
    { form
        | smaller =
            form.smaller
                |> (if index == (form.smaller |> Tuple.first |> Array.length) then
                        Tuple.mapSecond (always value)

                    else
                        Tuple.mapFirst
                            (Array.update index
                                (\{ name } ->
                                    { name = name
                                    , refinement = value
                                    }
                                )
                            )
                   )
    }


setBigger : Int -> String -> ConditionForm -> ConditionForm
setBigger index value form =
    { form
        | bigger =
            form.bigger
                |> (if index == (form.smaller |> Tuple.first |> Array.length) then
                        Tuple.mapSecond (always value)

                    else
                        Tuple.mapFirst
                            (Array.update index
                                (\{ name } ->
                                    { name = name
                                    , refinement = value
                                    }
                                )
                            )
                   )
    }


setVariable : Int -> String -> ConditionForm -> ConditionForm
setVariable index value form =
    let
        fun =
            if index == -1 then
                Tuple.mapSecond (always value)

            else
                Tuple.mapFirst
                    (Array.update index
                        (\{ refinement } ->
                            { name = value
                            , refinement = refinement
                            }
                        )
                    )
    in
    { form
        | bigger = fun form.bigger
        , smaller = fun form.smaller
    }


setTypeVariables : Int -> String -> ConditionForm -> ConditionForm
setTypeVariables index value form =
    { form
        | typeVariables =
            form.typeVariables
                |> Array.update index
                    (\( k, _ ) -> ( k, value ))
    }


setTypeVariableName : Int -> String -> ConditionForm -> ConditionForm
setTypeVariableName index value form =
    { form
        | typeVariables =
            form.typeVariables
                |> Array.update index
                    (\( _, v ) -> ( value, v ))
    }


setGuard : Int -> String -> ConditionForm -> ConditionForm
setGuard index value form =
    { form
        | guards =
            form.guards
                |> Array.set index value
    }


emptyForm : ConditionForm
emptyForm =
    { smaller = ( Array.empty, "True" )
    , bigger = ( Array.empty, "[k1]_{}" )
    , guards = Array.empty
    , typeVariables = Array.empty
    }


decode : ConditionForm -> Result String Condition
decode { smaller, bigger, guards, typeVariables } =
    Result.map4 Condition
        (smaller
            |> (\( list, head ) ->
                    Result.map2
                        (\a b -> ( a, b ))
                        (list
                            |> Array.toList
                            |> List.map
                                (\{ name, refinement } ->
                                    refinement
                                        |> Template.decode
                                        |> Result.map
                                            (\b ->
                                                { name = name
                                                , refinement = b
                                                }
                                            )
                                )
                            |> Result.combine
                        )
                        (head |> LiquidType.decodeSimpleLiquidType)
               )
        )
        (bigger
            |> (\( list, head ) ->
                    Result.map2
                        (\a b -> ( a, b ))
                        (list
                            |> Array.toList
                            |> List.map
                                (\{ name, refinement } ->
                                    refinement
                                        |> Refinement.decode
                                        |> Result.map
                                            (\b ->
                                                { name = name
                                                , refinement = b
                                                }
                                            )
                                )
                            |> Result.combine
                        )
                        (head
                            |> Parser.run Template.decoder
                            |> Result.mapError Refinement.deadEndsToString
                        )
               )
        )
        (guards
            |> Array.map Refinement.decode
            |> Array.toList
            |> Result.combine
        )
        (typeVariables
            |> Array.map (Tuple.mapSecond Refinement.decode)
            |> Array.toList
            |> List.unzip
            |> (\( keys, values ) ->
                    values
                        |> Result.combine
                        |> Result.map (List.zip keys)
               )
        )


toSMTStatement : Dict Int Refinement -> SimpleCondition -> String
toSMTStatement dict { smaller, bigger, guards, typeVariables } =
    let
        typeVariablesRefinements : List Refinement
        typeVariablesRefinements =
            typeVariables
                |> List.map
                    (\( b, r ) ->
                        r |> Refinement.rename { find = "v", replaceWith = b }
                    )

        r1 : Refinement
        r1 =
            case smaller of
                IntType refinement ->
                    refinement

                LiquidTypeVariable ( int, list ) ->
                    list
                        |> List.foldl
                            (\( k, v ) ->
                                Refinement.substitute
                                    { find = k
                                    , replaceWith = v
                                    }
                            )
                            (dict
                                |> Dict.get int
                                |> Maybe.withDefault IsFalse
                            )

        r2 : Refinement
        r2 =
            bigger
                |> Tuple.second
                |> List.foldl
                    (\( k, v ) ->
                        Refinement.substitute
                            { find = k
                            , replaceWith = v
                            }
                    )
                    (dict
                        |> Dict.get (bigger |> Tuple.first)
                        |> Maybe.withDefault IsFalse
                    )

        statement : Refinement
        statement =
            (r1
                :: typeVariablesRefinements
                ++ guards
            )
                |> List.foldl AndAlso (IsNot r2)
    in
    (statement
        |> Refinement.variables
        |> Set.toList
        |> List.map (\k -> "(declare-const " ++ k ++ " Int)\n")
        |> String.concat
    )
        ++ ("(assert " ++ (statement |> Refinement.toSMTStatement) ++ ")\n(check-sat)")
