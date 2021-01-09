module Main exposing (main)

import Action
import Browser
import Element
import Element.Font as Font
import Html exposing (Html)
import Page.Assistant as Assistant
import Page.Setup as Setup


type Model
    = Setup Setup.Model
    | Assistant Assistant.Model


type Msg
    = WhileSetup Setup.Msg
    | WhileAssistant Assistant.Msg


init : () -> ( Model, Cmd Msg )
init () =
    ( Setup.init |> Setup
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update mg ml =
    case ( mg, ml ) of
        ( WhileSetup msg, Setup model ) ->
            Setup.update msg model
                |> Action.config
                |> Action.withTransition Assistant.init Assistant WhileAssistant
                |> Action.withUpdate Setup WhileSetup
                |> Action.apply

        ( WhileAssistant msg, Assistant model ) ->
            Assistant.update msg model
                |> Tuple.mapBoth Assistant (Cmd.map WhileAssistant)

        ( _, _ ) ->
            ( ml, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view m =
    (case m of
        Setup model ->
            Setup.view model |> List.map (Element.map WhileSetup)

        Assistant model ->
            Assistant.view model |> List.map (Element.map WhileAssistant)
    )
        |> Element.column
            [ Element.centerX
            , Element.width <|
                Element.px <|
                    800
            , Element.spacing
                10
            ]
        |> Element.layout [ Font.size 16 ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
