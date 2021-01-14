module Page.Done exposing (Model, Transition, init, view)

import Array exposing (Array)
import Color
import Data.Algorithm as Algorithm
import Data.Condition as Condition exposing (Condition, ConditionForm, SimpleCondition)
import Data.LiquidType as LiquidType exposing (Input(..))
import Data.Refinement as Refinement exposing (Refinement(..))
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Framework.Grid as Grid
import Html exposing (Html)
import Result.Extra as Result
import Set exposing (Set)
import View.Condition as Condition
import View.ConditionForm as ConditionForm
import Widget
import Widget.Style.Material as Material
import Widget.Style.Material.Typography as Typography


type alias Model =
    { conditions : Array SimpleCondition
    , predicates : Dict Int Refinement
    }


type alias Transition =
    Model


init : Transition -> ( Model, Cmd msg )
init model =
    ( model
    , Cmd.none
    )


view : Model -> List (Element msg)
view model =
    [ [ "Result"
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
      , [ "Solution"
            |> Element.text
            |> Element.el Typography.h6
        ]
            ++ (model.predicates
                    |> Dict.toList
                    |> List.concatMap
                        (\( int, r ) ->
                            [ "kappa_"
                                ++ String.fromInt int
                                |> Element.text
                                |> Element.el Typography.subtitle2
                            , r
                                |> Refinement.toString
                                |> Element.text
                                |> List.singleton
                                |> Element.paragraph []
                            ]
                        )
               )
      ]
        |> List.map
            (Element.column Grid.simple)
        |> Widget.column (Material.cardColumn Material.defaultPalette)
    ]
