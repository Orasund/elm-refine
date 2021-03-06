module View.Condition exposing (view, viewSimple)

import Data.Condition exposing (Condition, SimpleCondition)
import Data.LiquidType as LiquidType
import Data.Refinement as Refinement
import Data.Template as Template
import Element exposing (Element)
import Framework.Grid as Grid


viewSimple : SimpleCondition -> Element msg
viewSimple { smaller, bigger, guards, typeVariables } =
    { smaller = ( [], smaller )
    , bigger = ( [], bigger )
    , guards = guards
    , typeVariables = typeVariables
    }
        |> view


view : Condition -> Element msg
view { smaller, bigger, guards, typeVariables } =
    [ smaller
        |> LiquidType.toString Template.toString LiquidType.simpleLiquidTypeToString
        |> Element.text
    , " <: " |> Element.text
    , bigger |> LiquidType.toString Refinement.toString Template.toString |> Element.text
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
                                ++ (t |> Refinement.toString |> LiquidType.simpleformToString)
                        )
                    |> String.join ","
               )
            |> Element.text
    ]
        |> Element.paragraph Grid.simple
