module View.ConditionForm exposing (..)

import Array exposing (Array)
import Data.Condition as Condition exposing (ConditionForm)
import Data.LiquidType as LiquidType exposing (Input(..))
import Element exposing (Element)
import Element.Input as Input
import Framework.Grid as Grid
import Widget
import Widget.Style.Material as Material
import Widget.Style.Material.Typography as Typography


viewInput : { label : String, onChange : String -> msg } -> String -> Element msg
viewInput { label, onChange } text =
    [ label ++ " = " |> Element.text
    , Input.text []
        { onChange = onChange
        , text = text
        , placeholder = Nothing
        , label = Input.labelHidden label
        }
    ]
        |> Element.row Grid.spacedEvenly


viewInputArray : String -> (Int -> String -> msg) -> ( Array String, String ) -> Element msg
viewInputArray label onChange ( tail, head ) =
    let
        fun index input =
            input
                |> viewInput
                    { label = label ++ String.fromInt (index + 1)
                    , onChange = onChange index
                    }
    in
    (head |> fun -1)
        :: (tail |> Array.indexedMap fun |> Array.toList)
        |> Element.column Grid.simple


view :
    { onChangedSmaller : Int -> String -> msg
    , onChangedBigger : Int -> String -> msg
    , onChangedTypeVariables : Int -> String -> msg
    , onChangedGuard : Int -> String -> msg
    , addType : msg
    , removeType : msg
    , addGuard : msg
    , removeGuard : msg
    , addTypeVariable : msg
    , removeTypeVariable : msg
    }
    -> ConditionForm
    -> List (Element msg)
view msg { smaller, bigger, guards, typeVariables } =
    [ [ "T1 <: T2" |> Element.text |> Element.el Typography.h6
      , ("T1 := " ++ (smaller |> LiquidType.formToString "x"))
            |> Element.text
            |> Element.el Typography.subtitle2
      , smaller.baseType |> viewInputArray "x" msg.onChangedSmaller
      , ("T2 := " ++ (bigger |> LiquidType.formToString "y"))
            |> Element.text
            |> Element.el Typography.subtitle2
      , bigger.baseType |> viewInputArray "y" msg.onChangedBigger
      , (if bigger.baseType |> Tuple.first |> Array.isEmpty then
            []

         else
            Widget.button (Material.textButton Material.defaultPalette)
                { text = "Remove"
                , icon = Element.none
                , onPress = Just msg.removeType
                }
                |> List.singleton
        )
            ++ [ Widget.button (Material.outlinedButton Material.defaultPalette)
                    { text = "Add"
                    , icon = Element.none
                    , onPress = Just msg.addType
                    }
               ]
            |> Element.row [ Element.alignRight ]
      ]
    , [ "Guards" |> Element.text |> Element.el Typography.h6
      , guards
            |> Array.indexedMap
                (\index value ->
                    Input.text []
                        { onChange = msg.onChangedGuard index
                        , text = value
                        , placeholder = Nothing
                        , label = Input.labelHidden ("Guard " ++ String.fromInt index)
                        }
                )
            |> Array.toList
            |> Element.column Grid.simple
      , (if guards |> Array.isEmpty then
            []

         else
            Widget.button (Material.textButton Material.defaultPalette)
                { text = "Remove"
                , icon = Element.none
                , onPress = Just msg.removeGuard
                }
                |> List.singleton
        )
            ++ [ Widget.button (Material.outlinedButton Material.defaultPalette)
                    { text = "Add"
                    , icon = Element.none
                    , onPress = Just msg.addGuard
                    }
               ]
            |> Element.row [ Element.alignRight ]
      ]
    , [ "type variables" |> Element.text |> Element.el Typography.h6
      , typeVariables
            |> Array.indexedMap
                (\index ( name, t ) ->
                    t
                        |> viewInput
                            { label = name
                            , onChange = msg.onChangedTypeVariables index
                            }
                )
            |> Array.toList
            |> Element.column Grid.simple
      , (if typeVariables |> Array.isEmpty then
            []

         else
            Widget.button (Material.textButton Material.defaultPalette)
                { text = "Remove"
                , icon = Element.none
                , onPress = Just msg.removeTypeVariable
                }
                |> List.singleton
        )
            ++ [ Widget.button (Material.outlinedButton Material.defaultPalette)
                    { text = "Add"
                    , icon = Element.none
                    , onPress = Just msg.addTypeVariable
                    }
               ]
            |> Element.row [ Element.alignRight ]
      ]
    ]
        |> List.map
            (Element.column Grid.simple)
