module Main exposing (main)

import Browser
import Data.Condition as Condition exposing (Condition, ConditionForm)
import Data.LiquidType as LiquidType exposing (Input(..))
import Dict
import Element exposing (Element)
import Framework.Grid as Grid
import Framework.Heading as Heading
import Html exposing (Html)
import Widget
import Widget.Style.Material as Material


type alias Model =
    { conditions : List Condition
    , form : ConditionForm
    }


type Msg
    = PressedAddCondition


init : () -> ( Model, Cmd Msg )
init () =
    ( { conditions = []
      , form = Condition.emptyForm
      }
    , Cmd.none
    )


viewInputList : ( Input, List Input ) -> Element Msg
viewInputList ( head, tail ) =
    head
        :: tail
        |> List.map
            (\input ->
                (case input of
                    IntegerInput int ->
                        int |> String.fromInt

                    StringInput string ->
                        string
                )
                    |> Element.text
            )
        |> Element.column Grid.simple


viewForm : ConditionForm -> Element Msg
viewForm { smaller, bigger, guards, typeVariables } =
    [ smaller |> viewInputList
    , bigger |> viewInputList
    , guards |> List.map Element.text |> Element.column Grid.simple
    , typeVariables
        |> List.map
            (\( name, t ) ->
                [ name
                    |> Element.text
                    |> Element.el Heading.h2
                , t |> viewInputList
                ]
                    |> Element.column Grid.simple
            )
        |> Element.column Grid.simple
    ]
        |> Element.column Grid.simple


view : Model -> Html Msg
view model =
    [ "Conditions"
        |> Element.text
        |> Element.el Heading.h1
    ]
        ++ model.conditions viewConditions
        ++ [ viewForm model.form ]
        ++ [ Widget.button (Material.containedButton Material.defaultPalette)
                { text = "Add"
                , icon = Element.none
                , onPress = Just PressedAddCondition
                }
           ]
        |> Element.column [ Element.centerX ]
        |> Element.layout []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressedAddCondition ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
