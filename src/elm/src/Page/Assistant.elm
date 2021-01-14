module Page.Assistant exposing (Model, Msg(..), Transition, init, update, view)

import Action exposing (Action)
import Array exposing (Array)
import Array.Extra as Array
import Color
import Data.Algorithm as Algorithm
import Data.Condition as Condition exposing (Condition, ConditionForm, SimpleCondition)
import Data.IncomingMsg exposing (IncomingMsg)
import Data.LiquidType as LiquidType exposing (Input(..))
import Data.Refinement as Refinement exposing (IntExp, Refinement(..))
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Framework.Grid as Grid
import Html exposing (Html)
import List.Extra exposing (find)
import Page.Done as Done
import Result.Extra as Result
import Set exposing (Set)
import View.Condition as Condition
import View.ConditionForm as ConditionForm
import Widget
import Widget.Style.Material as Material
import Widget.Style.Material.Typography as Typography


type alias Model =
    { conditions : Array SimpleCondition
    , predicates : Dict Int (Array Refinement)
    , index : Int
    , weaken :
        Maybe
            { index : Int
            , liquidTypeVariable : Int
            }
    }


getLazySubstitute : Model -> List ( String, IntExp )
getLazySubstitute { conditions, index } =
    conditions
        |> Array.get index
        |> Maybe.map (.bigger >> Tuple.second)
        |> Maybe.withDefault []


smtStatement : Model -> Maybe String
smtStatement model =
    model.conditions
        |> Array.get model.index
        |> Maybe.map
            (\condition ->
                case model.weaken of
                    Just weaken ->
                        condition
                            |> Condition.toSMTStatement
                                (model.conditions
                                    |> Array.toList
                                    |> List.map
                                        (\{ typeVariables } ->
                                            typeVariables
                                                |> List.map (\( name, _ ) -> name)
                                        )
                                    |> List.concat
                                )
                                (model.predicates
                                    |> Dict.map (\_ -> Array.toList >> Refinement.conjunction)
                                    |> Dict.update (condition.bigger |> Tuple.first)
                                        (Maybe.map
                                            (\_ ->
                                                model
                                                    |> getLazySubstitute
                                                    |> List.foldl
                                                        (\( find, replaceWith ) ->
                                                            Refinement.substitute
                                                                { find = find
                                                                , replaceWith = replaceWith
                                                                }
                                                        )
                                                        (model.predicates
                                                            |> Dict.get (condition.bigger |> Tuple.first)
                                                            |> Maybe.andThen (Array.get weaken.index)
                                                            |> Maybe.withDefault IsFalse
                                                        )
                                            )
                                        )
                                )

                    Nothing ->
                        condition
                            |> Condition.toSMTStatement
                                (model.conditions
                                    |> Array.toList
                                    |> List.map
                                        (\{ typeVariables } ->
                                            typeVariables
                                                |> List.map (\( name, _ ) -> name)
                                        )
                                    |> List.concat
                                )
                                (model.predicates
                                    |> Dict.map (\_ -> Array.toList >> Refinement.conjunction)
                                )
            )


type Msg
    = GotResponse Bool
    | IncomingMsg IncomingMsg
    | AskSMT


type alias Transition =
    List SimpleCondition


type alias Update msg =
    Action Model
        msg
        Done.Transition
        --Allow Transitions?
        Never


init : Transition -> ( Model, Cmd Msg )
init conditions =
    let
        initList =
            (conditions
                |> List.map
                    (\{ typeVariables } ->
                        typeVariables
                            |> List.map (\( name, _ ) -> name)
                    )
                |> List.concat
            )
                |> Refinement.init
    in
    ( { conditions = conditions |> Array.fromList
      , predicates =
            conditions
                |> List.concatMap Condition.liquidTypeVariables
                |> List.map (\v -> ( v, initList |> Array.fromList ))
                |> Dict.fromList
      , index = 0
      , weaken = Nothing
      }
    , Cmd.none
    )


handleResponse : Bool -> Model -> Update msg
handleResponse bool model =
    case ( model.weaken, bool ) of
        ( Just weaken, False ) ->
            --continue
            let
                index =
                    weaken.index + 1
            in
            if
                index
                    >= (model.predicates
                            |> Dict.get weaken.liquidTypeVariable
                            |> Maybe.map Array.length
                            |> Maybe.withDefault 0
                       )
            then
                Action.updating
                    ( { model
                        | weaken = Nothing
                        , index = 0
                      }
                    , Cmd.none
                    )

            else
                Action.updating
                    ( { model
                        | weaken =
                            Just
                                { liquidTypeVariable = weaken.liquidTypeVariable
                                , index = index
                                }
                      }
                    , Cmd.none
                    )

        ( Just weaken, True ) ->
            --remove
            let
                predicates =
                    model.predicates
                        |> Dict.update weaken.liquidTypeVariable
                            (Maybe.map
                                (Array.removeAt weaken.index)
                            )
            in
            if
                weaken.index
                    >= (predicates
                            |> Dict.get weaken.liquidTypeVariable
                            |> Maybe.map Array.length
                            |> Maybe.withDefault 0
                       )
            then
                Action.updating
                    ( { model
                        | predicates = predicates
                        , weaken = Nothing
                        , index = 0
                      }
                    , Cmd.none
                    )

            else
                Action.updating
                    ( { model
                        | predicates = predicates
                      }
                    , Cmd.none
                    )

        ( Nothing, True ) ->
            --Start weaking
            case
                model.conditions
                    |> Array.get model.index
            of
                Just { bigger } ->
                    Action.updating
                        ( { model
                            | weaken =
                                Just
                                    { index = 0
                                    , liquidTypeVariable = bigger |> Tuple.first
                                    }
                          }
                        , Cmd.none
                        )

                Nothing ->
                    Action.updating ( model, Cmd.none )

        ( Nothing, False ) ->
            --continue
            let
                index =
                    model.index + 1
            in
            if index >= (model.conditions |> Array.length) then
                Action.transitioning
                    { conditions = model.conditions
                    , predicates =
                        model.predicates
                            |> Dict.map (\_ -> Array.toList >> Refinement.conjunction)
                    }

            else
                Action.updating
                    ( { model
                        | index = index
                      }
                    , Cmd.none
                    )


update : (String -> Cmd msg) -> Msg -> Model -> Update msg
update sendMsg msg model =
    case msg of
        GotResponse bool ->
            handleResponse bool model

        IncomingMsg ({ kind, payload } as m) ->
            if kind == "VERIFICATION_COMPLETE" then
                case payload |> Debug.log "response" of
                    ".true" ->
                        handleResponse True model

                    ".false" ->
                        handleResponse False model

                    _ ->
                        Action.updating
                            ( model
                            , Cmd.none
                            )

            else
                let
                    _ =
                        m
                            |> Debug.log "response"
                in
                Action.updating
                    ( model
                    , Cmd.none
                    )

        AskSMT ->
            Action.updating
                ( model, model |> smtStatement |> Maybe.map sendMsg |> Maybe.withDefault Cmd.none )


view : Model -> List (Element Msg)
view model =
    [ [ "Proof Assistant"
            |> Element.text
            |> Element.el Typography.h5
            |> List.singleton
      , [ "Conditions"
            |> Element.text
            |> Element.el Typography.h6
        ]
            ++ (model.conditions
                    |> Array.map Condition.viewSimple
                    |> Array.toList
               )
      , [ "Partial Solution"
            |> Element.text
            |> Element.el Typography.h6
        ]
            ++ (model.predicates
                    |> Dict.toList
                    |> List.concatMap
                        (\( int, array ) ->
                            [ "kappa_"
                                ++ String.fromInt int
                                |> Element.text
                                |> Element.el Typography.subtitle2
                            , array
                                |> Array.toList
                                |> List.map Refinement.toString
                                |> String.join ", "
                                |> Element.text
                                |> List.singleton
                                |> Element.paragraph []
                            ]
                        )
               )
      , [ "Is the following SMT statement satisfiable? ("
            ++ (case model.weaken of
                    Just weaken ->
                        "Weaken: part "
                            ++ String.fromInt (weaken.index + 1)
                            ++ "/"
                            ++ (model.predicates
                                    |> Dict.get weaken.liquidTypeVariable
                                    |> Maybe.map Array.length
                                    |> Maybe.withDefault 0
                                    |> String.fromInt
                               )

                    Nothing ->
                        "Solve: part "
                            ++ String.fromInt (model.index + 1)
                            ++ "/"
                            ++ (model.conditions |> Array.length |> String.fromInt)
               )
            ++ ")"
            |> Element.text
            |> Element.el Typography.h6
        , model
            |> smtStatement
            |> Maybe.map
                (Element.text
                    >> List.singleton
                    >> Element.paragraph []
                )
            |> Maybe.withDefault Element.none
        , [ Widget.button (Material.containedButton Material.defaultPalette)
                { text = "Ask SMT Solver"
                , icon = Element.none
                , onPress = Just <| AskSMT
                }
          , [ Widget.button (Material.textButton Material.defaultPalette)
                { text = "No"
                , icon = Element.none
                , onPress = Just <| GotResponse False
                }
            , Widget.button (Material.outlinedButton Material.defaultPalette)
                { text = "Yes"
                , icon = Element.none
                , onPress = Just <| GotResponse True
                }
            ]
                |> Element.row []
          ]
            |> Element.row [ Element.spaceEvenly, Element.width <| Element.fill ]
        ]
      ]
        |> List.map
            (Element.column Grid.simple)
        |> Widget.column (Material.cardColumn Material.defaultPalette)
    ]
