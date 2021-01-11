module Data.Algorithm exposing (split)

import Data.Condition exposing (Condition, SimpleCondition)
import Data.LiquidType exposing (SimpleLiquidType(..))
import Data.Refinement exposing (Refinement)
import Data.Template exposing (Template)
import Dict exposing (Dict)


split : Condition -> Result () (List SimpleCondition)
split =
    let
        rec : Int -> Condition -> Result () (List SimpleCondition)
        rec offset condition =
            case ( condition.smaller.baseType, condition.bigger.baseType ) of
                ( ( q1 :: t2, t2end ), ( q3 :: t4, t4end ) ) ->
                    rec (offset + 1)
                        { condition
                            | smaller =
                                { name = condition.smaller.name
                                , baseType = ( t2, t2end )
                                }
                            , bigger =
                                { name = condition.smaller.name
                                , baseType = ( t4, t4end )
                                }
                            , typeVariables =
                                ( condition.smaller.name ++ String.fromInt offset, q3 )
                                    :: condition.typeVariables
                        }
                        |> Result.map
                            ((::)
                                { smaller = IntType q3
                                , bigger = q1
                                , guards = condition.guards
                                , typeVariables = condition.typeVariables
                                }
                            )

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
    \c ->
        if c.smaller.name == c.bigger.name then
            c |> rec 0

        else
            Err ()
