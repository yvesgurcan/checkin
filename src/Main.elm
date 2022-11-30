
module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Styled.Attributes exposing (css)
import List exposing (..)
import Date exposing (..)
import Css exposing (..)

type alias Model = 
    List EmotionalCheckIn

type alias EmotionalCheckIn =
    { status : String
    , lastUpdated : Int
    }

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : () -> (Model, Cmd Msg)
init flags =
    ([{ status = ":)", lastUpdated = 100 }], Cmd.none)


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
subscriptions _ =
    Sub.none

type alias FirstModelEntry = EmotionalCheckIn

firstModelEntry : Model -> FirstModelEntry
firstModelEntry model = case (head model) of
    Just val -> val
    Nothing -> { status = "", lastUpdated = 0 }

view : Model -> Html msg
view model =
    main_[-- css [ color (hex "ffffff")
           -- ,  backgroundColor (hex "000000")
           -- , Css.height (vh 100) ]
           ] [
        h1 [ --css [ margin (px 0) ]
        ]
        [ text "Emotional availabilty check-in" ]

        , input [ value (.status (firstModelEntry model))] []
            
        ]
