module Page.Setup exposing (Model, Msg(..), init, update, view)

import Action exposing (Action)
import Color
import Data.Condition as Condition exposing (Condition, ConditionForm)
import Data.LiquidType exposing (Input(..))
import Element exposing (Element)
import Element.Font as Font
import Page.Assistant as Assistant
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
    , form = Condition.emptyForm
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

        ChangedTypeVariables i1 i2 string ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.setTypeVariables ( i1, i2 ) string
                  }
                , Cmd.none
                )

        AddSmaller ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.addSmaller
                  }
                , Cmd.none
                )

        RemoveSmaller ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.removeSmaller
                  }
                , Cmd.none
                )

        AddBigger ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.addBigger
                  }
                , Cmd.none
                )

        RemoveBigger ->
            Action.updating
                ( { model
                    | form =
                        model.form
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

        AddAtTypeVariable i ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.addAtTypeVariable i
                  }
                , Cmd.none
                )

        RemoveAtTypeVariable i ->
            Action.updating
                ( { model
                    | form =
                        model.form
                            |> Condition.removeAtTypeVariable i
                  }
                , Cmd.none
                )

        StartProving ->
            Action.transitioning model.conditions


view : Model -> List (Element Msg)
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
        ++ [ [ "Conditions"
                |> Element.text
                |> Element.el Typography.h5
             ]
                ++ (model.conditions
                        |> List.map Condition.view
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
                |> List.singleton
             ]
                |> List.concat
                |> Widget.column (Material.cardColumn Material.defaultPalette)
           ]
