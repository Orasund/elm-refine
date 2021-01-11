module Page.Setup exposing (Model, Msg(..), init, update, view)

import Action exposing (Action)
import Color
import Data.Algorithm as Algorithm
import Data.Condition as Condition exposing (Condition, ConditionForm)
import Data.LiquidType exposing (Input(..))
import Element exposing (Element)
import Element.Font as Font
import Page.Assistant as Assistant
import Result.Extra as Result
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
    | ChangedTypeVariables Int String
    | AddType
    | RemoveType
    | AddGuard
    | RemoveGuard
    | AddTypeVariable
    | RemoveTypeVariable
    | StartProving


type alias Update =
    Action Model
        Msg
        Assistant.Transition
        --Allow Transitions?
        Never



--Allow Exit?


init : Model
init =
    { conditions = []
    , form = Condition.emptyForm 0
    , error = Nothing
    }


update : Msg -> Model -> Update
update msg model =
    case msg of
        PressedAddCondition ->
            case model.form |> Condition.decode of
                Ok condition ->
                    Action.updating
                        ( { model
                            | conditions =
                                model.conditions
                                    |> List.append [ condition ]
                            , form =
                                Condition.emptyForm
                                    (model.conditions
                                        |> List.length
                                        |> (+) 1
                                    )
                            , error = Nothing
                          }
                        , Cmd.none
                        )

                Err err ->
                    Action.updating
                        ( { model
                            | error = Just err
                          }
                        , Cmd.none
                        )

        ChangedBigger index string ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.setBigger index string
                  }
                , Cmd.none
                )

        ChangedSmaller index string ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.setSmaller index string
                  }
                , Cmd.none
                )

        ChangedGuard index string ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.setGuard index string
                  }
                , Cmd.none
                )

        ChangedTypeVariables index string ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.setTypeVariables index string
                  }
                , Cmd.none
                )

        AddType ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.addSmaller
                            |> Condition.addBigger
                  }
                , Cmd.none
                )

        RemoveType ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.removeSmaller
                            |> Condition.removeBigger
                  }
                , Cmd.none
                )

        AddGuard ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.addGuard
                  }
                , Cmd.none
                )

        RemoveGuard ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.removeGuard
                  }
                , Cmd.none
                )

        AddTypeVariable ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.addTypeVariable
                  }
                , Cmd.none
                )

        RemoveTypeVariable ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.removeTypeVariable
                  }
                , Cmd.none
                )

        StartProving ->
            case model.conditions |> List.map Algorithm.split |> Result.combine of
                Ok conds ->
                    conds |> List.concat |> Action.transitioning

                Err () ->
                    Action.updating
                        ( { model
                            | error = Just "variables names are not the same"
                          }
                        , Cmd.none
                        )


view : Model -> List (Element Msg)
view model =
    [ [ "Conditions"
            |> Element.text
            |> Element.el Typography.h5
      ]
        ++ (model.conditions
                |> List.map Condition.view
           )
        ++ (model.error
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
        ++ [ Widget.button (Material.containedButton Material.defaultPalette)
                { text = "Start Proving"
                , icon = Element.none
                , onPress = Just StartProving
                }
           ]
        |> Widget.column (Material.cardColumn Material.defaultPalette)
    , [ "Adding Condition"
            |> Element.text
            |> Element.el Typography.h5
            |> List.singleton
      , model.form
            |> ConditionForm.view
                { onChangedBigger = ChangedBigger
                , onChangedSmaller = ChangedSmaller
                , onChangedTypeVariables = ChangedTypeVariables
                , onChangedGuard = ChangedGuard
                , addType = AddType
                , removeType = RemoveType
                , addGuard = AddGuard
                , removeGuard = RemoveGuard
                , addTypeVariable = AddTypeVariable
                , removeTypeVariable = RemoveTypeVariable
                }
      , Widget.button (Material.containedButton Material.defaultPalette)
            { text = "Add Condition"
            , icon = Element.none
            , onPress = Just PressedAddCondition
            }
            |> List.singleton
      ]
        |> List.concat
        |> Widget.column (Material.cardColumn Material.defaultPalette)
    ]
