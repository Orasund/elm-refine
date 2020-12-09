module Data.Condition exposing (Condition, ConditionForm, decode, emptyForm)

import Data.LiquidType as LiquidType exposing (Input(..), IntExp(..), LiquidType, Refinement(..), SimpleLiquidType(..))
import Dict exposing (Dict)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import Result.Extra as Result
import Set


type alias Condition =
    { smaller : LiquidType
    , bigger : LiquidType
    , guards : List Refinement
    , typeVariales : Dict String LiquidType
    }


type alias ConditionForm =
    { smaller : ( Input, List Input )
    , bigger : ( Input, List Input )
    , guards : List String
    , typeVariables : List ( String, ( Input, List Input ) )
    }


emptyForm : ConditionForm
emptyForm =
    { smaller = ( StringInput "", [] )
    , bigger = ( StringInput "", [] )
    , guards = []
    , typeVariables = []
    }


decode : ConditionForm -> Result String Condition
decode { smaller, bigger, guards, typeVariables } =
    Result.map4 Condition
        (smaller |> LiquidType.decode)
        (bigger |> LiquidType.decode)
        (guards
            |> List.map LiquidType.decodeRefinement
            |> Result.combine
        )
        (typeVariables
            |> List.map (Tuple.mapSecond LiquidType.decode)
            |> List.unzip
            |> (\( keys, values ) ->
                    values
                        |> Result.combine
                        |> Result.map (List.zip keys >> Dict.fromList)
               )
        )
