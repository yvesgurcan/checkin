
module Main exposing (..)

import Browser
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)

import Date exposing (..)
import Css exposing (..)

type alias Model = 
    { status : String
    , lastUpdated : Int
    }

main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }

init : () -> ( Model, Cmd Msg )
init flags =
    ({ status = ":)", lastUpdated = 100 }, Cmd.none)


type Msg
    = Msg1
    | Msg2


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Msg1 ->
            (model, Cmd.none)

        Msg2 ->
            (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view msg = div[ css [ color (hex "ffffff"),  backgroundColor (hex "000000"), height (vh 100)]] [
    h1 [ css [  margin (px 0) ] ] [ text "Emotional availabilty check-in" ] ]
