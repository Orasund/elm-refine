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
    , removeBigger
    , removeGuard
    , removeSmaller
    , removeTypeVariable
    , setBigger
    , setGuard
    , setSmaller
    , setTypeVariables
    )

import Array exposing (Array)
import Array.Extra as Array
import Data.LiquidType as LiquidType exposing (Input(..), LiquidType, LiquidTypeForm, SimpleLiquidType(..))
import Data.Refinement exposing (IntExp(..), Refinement(..))
import Dict
import List.Extra as List
import Result.Extra as Result


type alias Condition =
    { smaller : LiquidType
    , bigger : LiquidType
    , guards : List Refinement
    , typeVariables : List ( String, SimpleLiquidType )
    }


type alias SimpleCondition =
    { smaller : SimpleLiquidType
    , bigger : SimpleLiquidType
    , guards : List Refinement
    , typeVariables : List ( String, SimpleLiquidType )
    }


addSmaller : ConditionForm -> ConditionForm
addSmaller form =
    { form
        | smaller =
            form.smaller
                |> (\t ->
                        { t
                            | baseType =
                                t.baseType
                                    |> Tuple.mapFirst (\l -> Array.append l ([ "True" ] |> Array.fromList))
                        }
                   )
    }


removeSmaller : ConditionForm -> ConditionForm
removeSmaller form =
    { form
        | smaller =
            form.smaller
                |> (\t ->
                        { t
                            | baseType =
                                t.baseType
                                    |> Tuple.mapFirst
                                        (\l ->
                                            l
                                                |> Array.removeAt (l |> Array.length |> (+) -1)
                                        )
                        }
                   )
    }


addBigger : ConditionForm -> ConditionForm
addBigger form =
    { form
        | bigger =
            form.bigger
                |> (\t ->
                        { t
                            | baseType =
                                t.baseType
                                    |> Tuple.mapFirst (\l -> Array.append l ([ "True" ] |> Array.fromList))
                        }
                   )
    }


removeBigger : ConditionForm -> ConditionForm
removeBigger form =
    { form
        | bigger =
            form.bigger
                |> (\t ->
                        { t
                            | baseType =
                                t.baseType
                                    |> Tuple.mapFirst
                                        (\l ->
                                            l
                                                |> Array.removeAt (l |> Array.length |> (+) -1)
                                        )
                        }
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
                |> (\t ->
                        { t
                            | baseType =
                                t.baseType
                                    |> (if index == -1 then
                                            Tuple.mapSecond (always value)

                                        else
                                            Tuple.mapFirst (Array.set index value)
                                       )
                        }
                   )
    }


setBigger : Int -> String -> ConditionForm -> ConditionForm
setBigger index value form =
    { form
        | bigger =
            form.bigger
                |> (\t ->
                        { t
                            | baseType =
                                t.baseType
                                    |> (if index == -1 then
                                            Tuple.mapSecond (always value)

                                        else
                                            Tuple.mapFirst (Array.set index value)
                                       )
                        }
                   )
    }


setTypeVariables : Int -> String -> ConditionForm -> ConditionForm
setTypeVariables index value form =
    { form
        | typeVariables =
            form.typeVariables
                |> Array.update index
                    (\( k, _ ) -> ( k, value ))
    }


setGuard : Int -> String -> ConditionForm -> ConditionForm
setGuard index value form =
    { form
        | guards =
            form.guards
                |> Array.set index value
    }


type alias ConditionForm =
    { smaller : LiquidTypeForm
    , bigger : LiquidTypeForm
    , guards : Array String
    , typeVariables : Array ( String, String )
    }


emptyForm : Int -> ConditionForm
emptyForm i =
    { smaller = { name = "b" ++ String.fromInt i, baseType = ( Array.empty, "True" ) }
    , bigger = { name = "b" ++ String.fromInt i, baseType = ( Array.empty, "True" ) }
    , guards = Array.empty
    , typeVariables = Array.empty
    }


decode : ConditionForm -> Result String Condition
decode { smaller, bigger, guards, typeVariables } =
    Result.map4 Condition
        (smaller |> LiquidType.decode)
        (bigger |> LiquidType.decode)
        (guards
            |> Array.map LiquidType.decodeRefinement
            |> Array.toList
            |> Result.combine
        )
        (typeVariables
            |> Array.map (Tuple.mapSecond LiquidType.decodeSimpleLiquidType)
            |> Array.toList
            |> List.unzip
            |> (\( keys, values ) ->
                    values
                        |> Result.combine
                        |> Result.map (List.zip keys)
               )
        )
