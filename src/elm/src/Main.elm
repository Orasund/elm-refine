port module Main exposing (main)

import Action
import Browser
import Data.IncomingMsg as IncomingMsg
import Element
import Element.Font as Font
import Html exposing (Html)
import Page.Assistant as Assistant exposing (Msg(..))
import Page.Done as Done
import Page.Setup as Setup exposing (Msg(..))


port incomingMsg : (( String, String ) -> msg) -> Sub msg


port outgoingMsg : String -> Cmd msg


type Model
    = Setup Setup.Model
    | Assistant Assistant.Model
    | Done Done.Model


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
            Assistant.update outgoingMsg msg model
                |> Action.config
                |> Action.withTransition Done.init Done identity
                |> Action.withUpdate Assistant WhileAssistant
                |> Action.apply

        ( _, _ ) ->
            ( ml, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Setup _ ->
            incomingMsg
                (\v ->
                    v
                        |> IncomingMsg.fromTuple
                        |> Setup.IncomingMsg
                        |> WhileSetup
                )

        Assistant _ ->
            incomingMsg
                (\v ->
                    v
                        |> IncomingMsg.fromTuple
                        |> Assistant.IncomingMsg
                        |> WhileAssistant
                )

        _ ->
            Sub.none


view : Model -> Html Msg
view m =
    (case m of
        Setup model ->
            Setup.view model |> List.map (Element.map WhileSetup)

        Assistant model ->
            Assistant.view model |> List.map (Element.map WhileAssistant)

        Done model ->
            Done.view model
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
