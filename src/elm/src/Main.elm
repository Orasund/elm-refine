module Main exposing (main)

import Browser
import Color
import Data.Condition as Condition exposing (Condition, ConditionForm)
import Data.LiquidType as LiquidType exposing (Input(..))
import Dict
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Framework.Grid as Grid
import Html exposing (Html)
import View.Condition as Condition
import View.ConditionForm as ConditionForm
import Widget
import Widget.Style.Material as Material
import Widget.Style.Material.Typography as Typography


type alias Model =
    { conditions : List Condition
    , form : ConditionForm
    , error : Maybe String
    }


type Msg
    = PressedAddCondition
    | ChangedBigger Int String
    | ChangedSmaller Int String
    | ChangedGuard Int String
    | ChangedTypeVariables Int Int String
    | AddSmaller
    | RemoveSmaller
    | AddBigger
    | RemoveBigger
    | AddGuard
    | RemoveGuard
    | AddTypeVariable
    | RemoveTypeVariable
    | AddAtTypeVariable Int
    | RemoveAtTypeVariable Int


init : () -> ( Model, Cmd Msg )
init () =
    ( { conditions = []
      , form = Condition.emptyForm
      , error = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressedAddCondition ->
            case model.form |> Condition.decode of
                Ok condition ->
                    ( { model
                        | conditions =
                            model.conditions
                                |> List.append [ condition ]
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model
                        | error = Just err
                      }
                    , Cmd.none
                    )

        ChangedBigger index string ->
            ( { model
                | form =
                    model.form
                        |> Condition.setBigger index string
              }
            , Cmd.none
            )

        ChangedSmaller index string ->
            ( { model
                | form =
                    model.form
                        |> Condition.setSmaller index string
              }
            , Cmd.none
            )

        ChangedGuard index string ->
            ( { model
                | form =
                    model.form
                        |> Condition.setGuard index string
              }
            , Cmd.none
            )

        ChangedTypeVariables i1 i2 string ->
            ( model |> Debug.todo "add Type Variable", Cmd.none )

        AddSmaller ->
            ( { model
                | form =
                    model.form
                        |> Condition.addSmaller
              }
            , Cmd.none
            )

        RemoveSmaller ->
            ( { model
                | form =
                    model.form
                        |> Condition.removeSmaller
              }
            , Cmd.none
            )

        AddBigger ->
            ( { model
                | form =
                    model.form
                        |> Condition.addBigger
              }
            , Cmd.none
            )

        RemoveBigger ->
            ( { model
                | form =
                    model.form
                        |> Condition.removeBigger
              }
            , Cmd.none
            )

        AddGuard ->
            ( { model
                | form =
                    model.form
                        |> Condition.addGuard
              }
            , Cmd.none
            )

        RemoveGuard ->
            ( { model
                | form =
                    model.form
                        |> Condition.removeGuard
              }
            , Cmd.none
            )

        AddTypeVariable ->
            ( { model
                | form =
                    model.form
                        |> Condition.addTypeVariable
              }
            , Cmd.none
            )

        RemoveTypeVariable ->
            ( { model
                | form =
                    model.form
                        |> Condition.removeTypeVariable
              }
            , Cmd.none
            )

        AddAtTypeVariable i ->
            ( { model
                | form =
                    model.form
                        |> Condition.addAtTypeVariable i
              }
            , Cmd.none
            )

        RemoveAtTypeVariable i ->
            ( { model
                | form =
                    model.form
                        |> Condition.removeAtTypeVariable i
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    (model.error
        |> Maybe.map
            (\string ->
                "Error : "
                    ++ string
                    |> Element.text
                    |> List.singleton
                    |> Element.paragraph
                        [ Font.color <|
                            Element.fromRgb <|
                                Color.toRgba <|
                                    Material.defaultPalette.error
                        ]
                    |> List.singleton
            )
        |> Maybe.withDefault []
    )
        ++ [ "Conditions"
                |> Element.text
                |> Element.el Typography.h2
           ]
        ++ (model.conditions
                |> List.map Condition.view
                |> Element.column Grid.simple
                |> List.singleton
           )
        ++ [ [ "Adding Condition: T1 <: T2 "
                |> Element.text
                |> Element.el Typography.h6
             , model.form
                |> ConditionForm.view
                    { onChangedBigger = ChangedBigger
                    , onChangedSmaller = ChangedSmaller
                    , onChangedTypeVariables = ChangedTypeVariables
                    , onChangedGuard = ChangedGuard
                    , addSmaller = AddSmaller
                    , removeSmaller = RemoveSmaller
                    , addBigger = AddBigger
                    , removeBigger = RemoveBigger
                    , addGuard = AddGuard
                    , removeGuard = RemoveGuard
                    , addTypeVariable = AddTypeVariable
                    , removeTypeVariable = RemoveTypeVariable
                    , addAtTypeVariable = AddAtTypeVariable
                    , removeAtTypeVariable = RemoveAtTypeVariable
                    }
             , Widget.button (Material.containedButton Material.defaultPalette)
                { text = "Add Condition"
                , icon = Element.none
                , onPress = Just PressedAddCondition
                }
             ]
                |> Widget.column (Material.cardColumn Material.defaultPalette)
           ]
        |> Element.column
            [ Element.centerX
            , Element.width <| Element.px <| 800
            ]
        |> Element.layout []


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
