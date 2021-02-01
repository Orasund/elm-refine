module Page.Assistant exposing (Model, Msg(..), Transition, init, update, view)

import Action exposing (Action)
import Array exposing (Array)
import Array.Extra as Array
import Color
import Data.Condition as Condition exposing (SimpleCondition)
import Data.IncomingMsg exposing (IncomingMsg)
import Data.IntExp exposing (IntExp)
import Data.LiquidType exposing (Input(..))
import Data.Refinement as Refinement exposing (Refinement(..))
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Framework.Grid as Grid
import Page.Done as Done
import View.Condition as Condition
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
    , auto : Bool
    , error : Maybe String
    }


getLazySubstitute : Model -> List ( String, IntExp )
getLazySubstitute { conditions, index } =
    conditions
        |> Array.get index
        |> Maybe.map (.bigger >> Tuple.second)
        |> Maybe.withDefault []


statementForSolve : Model -> SimpleCondition -> String
statementForSolve model condition =
    condition
        |> Condition.toSMTStatement
            (model.predicates
                |> Dict.map (\_ -> Array.toList >> Refinement.conjunction)
            )


statementForWeaken :
    { index : Int
    , liquidTypeVariable : Int
    }
    -> Model
    -> SimpleCondition
    -> String
statementForWeaken weaken model condition =
    condition
        |> Condition.toSMTStatement
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


smtStatement : Model -> Maybe String
smtStatement model =
    let
        toString : SimpleCondition -> String
        toString condition =
            case model.weaken of
                Just weaken ->
                    statementForWeaken weaken model condition

                Nothing ->
                    statementForSolve model condition
    in
    model.conditions
        |> Array.get model.index
        |> Maybe.map toString


type Msg
    = GotResponse Bool
    | IncomingMsg IncomingMsg
    | ToggleAuto
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
      , auto = False
      , error = Nothing
      }
    , Cmd.none
    )


handleAuto : (String -> Cmd msg) -> Model -> Update msg
handleAuto sendMsg model =
    if model.auto then
        ( model
        , model
            |> smtStatement
            |> Maybe.map sendMsg
            |> Maybe.withDefault Cmd.none
        )
            |> Action.updating

    else
        Action.updating
            ( model, Cmd.none )


handleSolve : (String -> Cmd msg) -> Bool -> Model -> Update msg
handleSolve sendMsg bool model =
    if bool then
        --Start weaking
        case
            model.conditions
                |> Array.get model.index
        of
            Just { bigger } ->
                { model
                    | weaken =
                        Just
                            { index = 0
                            , liquidTypeVariable = bigger |> Tuple.first
                            }
                }
                    |> handleAuto sendMsg

            Nothing ->
                Action.updating ( model, Cmd.none )

    else
        --Continue
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
            { model
                | index = index
            }
                |> handleAuto sendMsg


handleWeaken :
    { index : Int
    , liquidTypeVariable : Int
    }
    -> (String -> Cmd msg)
    -> Bool
    -> Model
    -> Update msg
handleWeaken weaken sendMsg bool model =
    if bool then
        --Remove
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
            { model
                | predicates = predicates
                , weaken = Nothing
                , index = 0
            }
                |> handleAuto sendMsg

        else
            { model
                | predicates = predicates
            }
                |> handleAuto sendMsg

    else
        --Continue
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
            { model
                | weaken = Nothing
                , index = 0
            }
                |> handleAuto sendMsg

        else
            { model
                | weaken =
                    Just
                        { liquidTypeVariable = weaken.liquidTypeVariable
                        , index = index
                        }
            }
                |> handleAuto sendMsg


handleResponse : (String -> Cmd msg) -> Bool -> Model -> Update msg
handleResponse sendMsg bool model =
    case model.weaken of
        Just weaken ->
            handleWeaken weaken sendMsg bool model

        Nothing ->
            handleSolve sendMsg bool model


update : (String -> Cmd msg) -> Msg -> Model -> Update msg
update sendMsg msg model =
    case msg of
        GotResponse bool ->
            handleResponse sendMsg bool { model | error = Nothing }

        IncomingMsg { kind, payload } ->
            if kind == "STDOUT" then
                case payload of
                    "sat" ->
                        handleResponse sendMsg True model

                    "unsat" ->
                        handleResponse sendMsg False model

                    _ ->
                        Action.updating
                            ( model
                            , Cmd.none
                            )

            else if kind == "STDERR" then
                Action.updating
                    ( { model
                        | error = Just payload
                      }
                    , Cmd.none
                    )

            else if kind == "VERIFICATION_COMPLETE" then
                Action.updating
                    ( model
                    , Cmd.none
                    )

            else
                Action.updating
                    ( { model | error = Just (kind ++ ":" ++ payload) }
                    , Cmd.none
                    )

        ToggleAuto ->
            Action.updating
                ( { model | auto = not model.auto }
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
      , ("Conditions"
            |> Element.text
            |> Element.el Typography.h6
        )
            :: (model.conditions
                    |> Array.map Condition.viewSimple
                    |> Array.toList
               )
      , ("Partial Solution"
            |> Element.text
            |> Element.el Typography.h6
        )
            :: (model.predicates
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
        , [ [ Widget.button (Material.containedButton Material.defaultPalette)
                { text = "Ask SMT Solver"
                , icon = Element.none
                , onPress = Just <| AskSMT
                }
            , Input.checkbox []
                { onChange = always ToggleAuto
                , icon = Input.defaultCheckbox
                , checked = model.auto
                , label = Input.labelHidden "Toggle Autorun"
                }
            , Element.text "auto"
            ]
                |> Element.row [ Element.spacing 10 ]
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
        , model.error
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
                )
            |> Maybe.withDefault Element.none
        ]
      ]
        |> List.map
            (Element.column Grid.simple)
        |> Widget.column (Material.cardColumn Material.defaultPalette)
    ]
