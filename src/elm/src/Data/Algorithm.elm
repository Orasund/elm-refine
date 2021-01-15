module Data.Algorithm exposing (split)

import Data.Condition exposing (Condition, SimpleCondition)
import Data.LiquidType exposing (SimpleLiquidType(..))


split : Condition -> Result () (List SimpleCondition)
split =
    let
        rec : Int -> Condition -> Result () (List SimpleCondition)
        rec offset condition =
            case ( condition.smaller, condition.bigger ) of
                ( ( q1 :: t2, t2end ), ( q3 :: t4, t4end ) ) ->
                    if q1.name == q3.name then
                        rec (offset + 1)
                            { condition
                                | smaller =
                                    ( t2, t2end )
                                , bigger =
                                    ( t4, t4end )
                                , typeVariables =
                                    ( q3.name, q3.baseType )
                                        :: condition.typeVariables
                            }
                            |> Result.map
                                ((::)
                                    { smaller = IntType q3.baseType
                                    , bigger = q1.baseType
                                    , guards = condition.guards
                                    , typeVariables = condition.typeVariables
                                    }
                                )

                    else
                        Err ()

                ( ( [], q1 ), ( [], q2 ) ) ->
                    [ { smaller = q1
                      , bigger = q2
                      , guards = condition.guards
                      , typeVariables = condition.typeVariables
                      }
                    ]
                        |> Ok

                _ ->
                    Err ()
    in
    rec 0
