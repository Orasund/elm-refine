module View.Condition exposing (view)

import Data.Condition exposing (Condition)
import Data.LiquidType as LiquidType exposing (LiquidType)
import Element exposing (Element)
import Framework.Grid as Grid


view : Condition -> Element msg
view { smaller, bigger, guards, typeVariables } =
    [ smaller |> LiquidType.toString |> Element.text
    , "<:" |> Element.text
    , bigger |> LiquidType.toString |> Element.text
    ]
        |> Element.row Grid.simple
