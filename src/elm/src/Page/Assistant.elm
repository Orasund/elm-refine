module Page.Assistant exposing (Model, Msg, Transition, init, update, view)

import Color
import Data.Algorithm as Algorithm
import Data.Condition as Condition exposing (Condition, ConditionForm, SimpleCondition)
import Data.LiquidType as LiquidType exposing (Input(..))
import Data.Refinement as Refinement exposing (Refinement)
import Dict
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
    { conditions : List SimpleCondition
    , predicates : List Refinement
    }


type Msg
    = Idle


type alias Transition =
    List SimpleCondition


init : Transition -> ( Model, Cmd Msg )
init conditions =
    ( { conditions = conditions
      , predicates =
            (conditions
                |> List.map
                    (\{ typeVariables } ->
                        typeVariables
                            |> List.length
                            |> List.range 1
                            |> List.map (String.fromInt >> (++) "a")
                    )
                |> List.concat
            )
                |> Refinement.init
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Idle ->
            ( model, Cmd.none )


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
                    |> List.map Condition.viewSimple
               )
      , [ "Partial Solution"
            |> Element.text
            |> Element.el Typography.h6
        , model.predicates
            |> List.map Refinement.toString
            |> String.join ", "
            |> Element.text
            |> List.singleton
            |> Element.paragraph []
        ]
      ]
        |> List.map
            (Element.column Grid.simple)
        |> Widget.column (Material.cardColumn Material.defaultPalette)
    ]
