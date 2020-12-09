module View.Condition exposing (view)

import Data.Condition exposing (Condition)
import Element exposing (Element)
import Data.Condition exposing (LiquidType)



view : Condition -> Element Msg
view { smaller, bigger, guards, typeVariables } =
    [ smaller |> ]
        |> Element.column Grid.simple
