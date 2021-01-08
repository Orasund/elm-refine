module Data.Condition exposing (Condition, ConditionForm, addAtTypeVariable, addBigger, addGuard, addSmaller, addTypeVariable, decode, emptyForm, removeAtTypeVariable, removeBigger, removeGuard, removeSmaller, removeTypeVariable, setBigger, setGuard, setSmaller)

import Array exposing (Array)
import Array.Extra as Array
import Data.LiquidType as LiquidType exposing (Input(..), IntExp(..), LiquidType, LiquidTypeForm, Refinement(..), SimpleLiquidType(..))
import Dict exposing (Dict)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import Result.Extra as Result
import Set


type alias Condition =
    { smaller : LiquidType
    , bigger : LiquidType
    , guards : List Refinement
    , typeVariables : Dict String LiquidType
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
                ([ ( "a"
                        ++ (form.typeVariables
                                |> Array.length
                                |> (+) 1
                                |> String.fromInt
                           )
                   , { name =
                        "c"
                            ++ (form.typeVariables
                                    |> Array.length
                                    |> (+) 1
                                    |> String.fromInt
                               )
                     , baseType = ( Array.empty, "True" )
                     }
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


addAtTypeVariable : Int -> ConditionForm -> ConditionForm
addAtTypeVariable i form =
    { form
        | typeVariables =
            form.typeVariables
                |> Array.update i
                    (\( var, { name, baseType } ) ->
                        ( var
                        , { name = name
                          , baseType =
                                baseType
                                    |> Tuple.mapFirst (\l -> Array.append l ([ "True" ] |> Array.fromList))
                          }
                        )
                    )
    }


removeAtTypeVariable : Int -> ConditionForm -> ConditionForm
removeAtTypeVariable i form =
    { form
        | typeVariables =
            form.typeVariables
                |> Array.update i
                    (\( var, { name, baseType } ) ->
                        ( var
                        , { name = name
                          , baseType =
                                baseType
                                    |> Tuple.mapFirst
                                        (\l ->
                                            l
                                                |> Array.removeAt (l |> Array.length |> (+) -1)
                                        )
                          }
                        )
                    )
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
    , typeVariables : Array ( String, LiquidTypeForm )
    }


emptyForm : ConditionForm
emptyForm =
    { smaller = { name = "b", baseType = ( Array.empty, "True" ) }
    , bigger = { name = "c", baseType = ( Array.empty, "True" ) }
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
            |> Array.map (Tuple.mapSecond LiquidType.decode)
            |> Array.toList
            |> List.unzip
            |> (\( keys, values ) ->
                    values
                        |> Result.combine
                        |> Result.map (List.zip keys >> Dict.fromList)
               )
        )
