module View.Condition exposing (view)

import Data.Condition exposing (Condition)
import Data.LiquidType as LiquidType exposing (LiquidType)
import Data.Refinement as Refinement
import Dict
import Element exposing (Element)
import Framework.Grid as Grid


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
    , if typeVariables |> Dict.isEmpty then
        Element.none

      else
        " where "
            ++ (typeVariables
                    |> Dict.toList
                    |> List.map
                        (\( v, t ) ->
                            v
                                ++ " in "
                                ++ (t |> LiquidType.toString)
                        )
                    |> String.join ","
               )
            |> Element.text
    ]
        |> Element.textColumn Grid.simple
