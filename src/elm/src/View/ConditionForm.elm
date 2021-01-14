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
    , Widget.textInput
        (Material.textInput Material.defaultPalette
            |> (\w -> { w | containerRow = w.containerRow ++ [ Element.width <| Element.fill ] })
        )
        { chips = []
        , text = text
        , placeholder = Nothing
        , label = label
        , onChange = onChange
        }
    ]
        |> Element.row Grid.spacedEvenly


viewVarInput : { onNameChange : String -> msg, onValueChange : String -> msg, name : String, value : String } -> Element msg
viewVarInput { onNameChange, onValueChange, name, value } =
    [ Widget.textInput
        (Material.textInput Material.defaultPalette
            |> (\w -> { w | containerRow = w.containerRow ++ [ Element.width <| Element.fill ] })
        )
        { chips = []
        , text = name
        , placeholder = Nothing
        , label = "Name"
        , onChange = onNameChange
        }
    , " = " |> Element.text |> Element.el [ Element.width <| Element.shrink ]
    , Widget.textInput
        (Material.textInput Material.defaultPalette
            |> (\w -> { w | containerRow = w.containerRow ++ [ Element.width <| Element.fill ] })
        )
        { chips = []
        , text = value
        , placeholder = Nothing
        , label = "Value"
        , onChange = onValueChange
        }
    ]
        |> Element.row Grid.spacedEvenly


viewInputArray : String -> (Int -> String -> msg) -> ( Array { name : String, baseType : String }, String ) -> Element msg
viewInputArray label onChange ( tail, head ) =
    let
        fun index input =
            input
                |> viewInput
                    { label = label ++ String.fromInt ((tail |> Array.length) - index)
                    , onChange = onChange index
                    }
    in
    (tail |> Array.indexedMap (\index { baseType } -> fun index baseType) |> Array.toList)
        ++ [ head |> fun (tail |> Array.length) ]
        |> Element.column Grid.simple


viewVarArray : String -> (Int -> String -> msg) -> Array { name : String, baseType : String } -> Element msg
viewVarArray label onChange tail =
    let
        fun index input =
            input
                |> viewInput
                    { label = label ++ String.fromInt ((tail |> Array.length) - index)
                    , onChange = onChange index
                    }
    in
    (tail |> Array.indexedMap (\index { name } -> fun index name) |> Array.toList)
        |> Element.column Grid.simple


view :
    { onChangedSmaller : Int -> String -> msg
    , onChangedBigger : Int -> String -> msg
    , onChangedVariable : Int -> String -> msg
    , onChangedTypeVariables : Int -> String -> msg
    , onChangedTypeVariableName : Int -> String -> msg
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
      , ("T1 := " ++ (smaller |> LiquidType.formToString "a" "x"))
            |> Element.text
            |> Element.el Typography.subtitle2
      , smaller |> viewInputArray "x" msg.onChangedSmaller
      , ("T2 := " ++ (bigger |> LiquidType.formToString "a" "y"))
            |> Element.text
            |> Element.el Typography.subtitle2
      , bigger |> viewInputArray "y" msg.onChangedBigger
      , (if bigger |> Tuple.first |> Array.isEmpty then
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
      , if bigger |> Tuple.first |> Array.isEmpty then
            Element.none

        else
            bigger
                |> Tuple.first
                |> viewVarArray "a" msg.onChangedVariable
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
                    viewVarInput
                        { name = name
                        , value = t
                        , onValueChange = msg.onChangedTypeVariables index
                        , onNameChange = msg.onChangedTypeVariableName index
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
