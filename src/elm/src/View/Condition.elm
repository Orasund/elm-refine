module View.Condition exposing (view, viewSimple)

import Data.Condition exposing (Condition, SimpleCondition)
import Data.LiquidType as LiquidType exposing (LiquidType)
import Data.Refinement as Refinement
import Dict
import Element exposing (Element)
import Framework.Grid as Grid


viewSimple : SimpleCondition -> Element msg
viewSimple { smaller, bigger, guards, typeVariables } =
    { smaller =
        { name = ""
        , baseType = ( [], smaller )
        }
    , bigger =
        { name = ""
        , baseType = ( [], bigger )
        }
    , guards = guards
    , typeVariables = typeVariables
    }
        |> view


view : Condition -> Element msg
view { smaller, bigger, guards, typeVariables } =
    [ smaller |> LiquidType.toString |> Element.text
    , " <: " |> Element.text
    , bigger |> LiquidType.toString |> Element.text
    , if guards |> List.isEmpty then
        Element.none

      else
        " with "
            ++ (guards |> List.map Refinement.toString |> String.join ",")
            |> Element.text
    , if typeVariables |> List.isEmpty then
        Element.none

      else
        " where "
            ++ (typeVariables
                    |> List.map
                        (\( name, t ) ->
                            name
                                ++ " in "
                                ++ (t |> LiquidType.simpleLiquidTypeToString)
                        )
                    |> String.join ","
               )
            |> Element.text
    ]
        |> Element.textColumn Grid.simple
