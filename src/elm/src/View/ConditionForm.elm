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


viewInputArray : String -> (Int -> String -> msg) -> ( Array String, String ) -> Element msg
viewInputArray label onChange ( tail, head ) =
    let
        fun index input =
            [ (label ++ String.fromInt (index + 1)) ++ " = " |> Element.text
            , Input.text []
                { onChange = onChange index
                , text = input
                , placeholder = Nothing
                , label = Input.labelHidden (label ++ String.fromInt (index + 1))
                }
            ]
                |> Element.row Grid.spacedEvenly
    in
    (head |> fun -1)
        :: (tail |> Array.indexedMap fun |> Array.toList)
        |> Element.column Grid.simple


view :
    { onChangedSmaller : Int -> String -> msg
    , onChangedBigger : Int -> String -> msg
    , onChangedTypeVariables : Int -> Int -> String -> msg
    , onChangedGuard : Int -> String -> msg
    , addSmaller : msg
    , removeSmaller : msg
    , addBigger : msg
    , removeBigger : msg
    , addGuard : msg
    , removeGuard : msg
    , addTypeVariable : msg
    , removeTypeVariable : msg
    , addAtTypeVariable : Int -> msg
    , removeAtTypeVariable : Int -> msg
    }
    -> ConditionForm
    -> Element msg
view msg { smaller, bigger, guards, typeVariables } =
    [ ("T1 := " ++ (smaller |> LiquidType.formToString "x"))
        |> Element.text
        |> Element.el Typography.h6
    , smaller.baseType |> viewInputArray "x" msg.onChangedSmaller
    , (if smaller.baseType |> Tuple.first |> Array.isEmpty then
        []

       else
        Widget.button (Material.textButton Material.defaultPalette)
            { text = "Remove"
            , icon = Element.none
            , onPress = Just msg.removeSmaller
            }
            |> List.singleton
      )
        ++ [ Widget.button (Material.outlinedButton Material.defaultPalette)
                { text = "Add"
                , icon = Element.none
                , onPress = Just msg.addSmaller
                }
           ]
        |> Element.row [ Element.alignRight ]
    , ("T2 := " ++ (smaller |> LiquidType.formToString "y"))
        |> Element.text
        |> Element.el Typography.h6
    , bigger.baseType |> viewInputArray "y" msg.onChangedBigger
    , (if bigger.baseType |> Tuple.first |> Array.isEmpty then
        []

       else
        Widget.button (Material.textButton Material.defaultPalette)
            { text = "Remove"
            , icon = Element.none
            , onPress = Just msg.removeBigger
            }
            |> List.singleton
      )
        ++ [ Widget.button (Material.outlinedButton Material.defaultPalette)
                { text = "Add"
                , icon = Element.none
                , onPress = Just msg.addBigger
                }
           ]
        |> Element.row [ Element.alignRight ]
    , "Guards" |> Element.text |> Element.el Typography.h6
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
    , "type variables" |> Element.text |> Element.el Typography.h6
    , typeVariables
        |> Array.indexedMap
            (\index ( name, t ) ->
                [ name
                    ++ " := "
                    ++ (t |> LiquidType.formToString ("z" ++ (String.fromInt <| index + 1)))
                    |> Element.text
                    |> Element.el Typography.subtitle2
                , t.baseType
                    |> viewInputArray ("z" ++ (index + 1 |> String.fromInt))
                        (msg.onChangedTypeVariables index)
                , (if t.baseType |> Tuple.first |> Array.isEmpty then
                    []

                   else
                    Widget.button (Material.textButton Material.defaultPalette)
                        { text = "Remove"
                        , icon = Element.none
                        , onPress = Just <| msg.removeAtTypeVariable index
                        }
                        |> List.singleton
                  )
                    ++ [ Widget.button (Material.outlinedButton Material.defaultPalette)
                            { text = "Add"
                            , icon = Element.none
                            , onPress = Just <| msg.addAtTypeVariable index
                            }
                       ]
                    |> Element.row [ Element.alignRight ]
                ]
                    |> Element.column Grid.simple
            )
        |> Array.toList
        |> Element.column Grid.simple
    , (if typeVariables |> Array.isEmpty then
        []

       else
        Widget.button (Material.textButton Material.defaultPalette)
            { text = "Remove Type Variable"
            , icon = Element.none
            , onPress = Just msg.removeTypeVariable
            }
            |> List.singleton
      )
        ++ [ Widget.button (Material.outlinedButton Material.defaultPalette)
                { text = "Add Type Variable"
                , icon = Element.none
                , onPress = Just msg.addTypeVariable
                }
           ]
        |> Element.row [ Element.alignRight ]
    ]
        |> Element.column Grid.simple
