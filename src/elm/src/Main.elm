module Main exposing (main)

import Browser
import Data.Condition exposing (Condition)
import Element
import Framework.Heading as Heading
import Html exposing (Html)


type alias Model =
    { conditions : List Condition
    }


type Msg
    = PressedAddCondition


init : () -> ( Model, Cmd Msg )
init () =
    ( { form = () }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    [ "Conditions"
        |> Element.text
        |> Element.el Heading.h1
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
