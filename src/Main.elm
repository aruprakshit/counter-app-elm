port module Main exposing (Model, Msg(..), initialModel, main, subscriptions, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E


main : Program (Maybe Int) Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Models


type alias Model =
    { count : Int
    , incBy : Int
    , decBy : Int
    }


type alias Scale =
    { x : Int
    , y : Int
    }


initialModel : Maybe Int -> ( Model, Cmd msg )
initialModel flags =
    case flags of
        Just flagVal ->
            ( Model flagVal 1 1, Cmd.none )

        Nothing ->
            ( Model 0 1 1, Cmd.none )



-- Messages


type Msg
    = Increment
    | Decrement
    | Received E.Value



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            let
                newCount =
                    model.count + model.incBy
            in
            ( { model | count = newCount }, toJs <| E.int newCount )

        Decrement ->
            let
                newCount =
                    model.count - model.decBy
            in
            ( { model | count = newCount }, toJs <| E.int newCount )

        Received val ->
            let
                record_ =
                    decodeCounterScale val
            in
            ( { model | incBy = record_.x, decBy = record_.y }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "width" "300px"
        , style "justify-content" "space-around"
        , style "background-color" "#ddd"
        , style "padding" "4em"
        , style "margin" "auto"
        , style "font-size" "20px"
        ]
        [ button
            [ onClick Increment
            , style "color" "#FFFFFF"
            , style "background-color" "#4CAF50"
            , style "font-weight" "800"
            , style "font-size" "1em"
            ]
            [ text ("+" ++ String.fromInt model.incBy) ]
        , div
            [ style "background-color" "#90caf9"
            , style "padding" "20px"
            , style "font-weight" "800"
            , style "color" "red"
            , style "font-size" "2em"
            ]
            [ text <| String.fromInt model.count ]
        , button
            [ onClick Decrement
            , style "color" "#FFFFFF"
            , style "background-color" "#4CAF50"
            , style "font-weight" "800"
            , style "font-size" "1em"
            ]
            [ text ("-" ++ String.fromInt model.decBy) ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    toElm Received



-- Decoders


counterScaleDecoder : D.Decoder Scale
counterScaleDecoder =
    D.map2 Scale
        (D.field "x" D.int)
        (D.field "y" D.int)


decodeCounterScale : D.Value -> Scale
decodeCounterScale value =
    case D.decodeValue counterScaleDecoder value of
        Ok scales ->
            scales

        Err _ ->
            Scale 1 1



-- Ports


port toJs : E.Value -> Cmd msg


port toElm : (E.Value -> msg) -> Sub msg
