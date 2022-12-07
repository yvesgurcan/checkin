
module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Styled.Attributes exposing (css)
import List exposing (..)
import Date exposing (..)
import Css exposing (..)

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = 
    List EmotionalCheckIn

type alias EmotionalCheckIn =
    { id: String
    , status : String
    , lastUpdated : Int
    }

type UpdatePayload
    = NoOp
    | SetEmotionalCheckInStatus String String
    
init : () -> ( Model, Cmd UpdatePayload )
init flags =
    (

        -- TODO: Use actual timestamp for the lastUpdated
        [ { id = "k", status = ":)", lastUpdated = 100 }
        , { id = "y", status = ":D", lastUpdated = 999 }]
    , Cmd.none
    )


update : UpdatePayload -> Model -> ( Model, Cmd UpdatePayload )
update payload model =
    case --Debug.log "update payload"
        payload of
        NoOp ->
            ( model, Cmd.none )

        SetEmotionalCheckInStatus id status ->
            let
                updateEmotionalCheckIn emotionalCheckIn =
                    if (emotionalCheckIn.id == id) then
                        { emotionalCheckIn | status = status }
                    else
                        emotionalCheckIn
                updatedModel = List.map updateEmotionalCheckIn model
            -- TODO: Change value of lastUpdated as well to be current timestamp.
            -- TODO: Send a POST request. Will need backend as well.
            in ( updatedModel, Cmd.none )


subscriptions : Model -> Sub UpdatePayload
subscriptions _ =
    Sub.none

getEmotionalCheckIn : String -> Model -> EmotionalCheckIn
getEmotionalCheckIn id model =
    let
        filterEmotionalCheckIn emotionalCheckIn = emotionalCheckIn.id == id
        matchEmotionalCheckIn = head ( filter filterEmotionalCheckIn model )
    in case matchEmotionalCheckIn of
        Just val -> val
        -- TODO: Let the function return Nothing if the record was not found instead of returning a misleadaing ID.
        Nothing -> { id = "BOOM", status = "", lastUpdated = 0 }

view : Model -> Html UpdatePayload
view model =
    let
        -- TODO: Output entire model instead of a specific string.
        _ = Debug.log "k" ( .status ( getEmotionalCheckIn "k" model ) )
        _ = Debug.log "y" ( .status ( getEmotionalCheckIn "y" model ) )
    in
    main_[]
        -- TODO: Style the view.
        [ h1 [] [ text "Emotional availabilty check-in" ]
        , label [] [ text "K's emotional availability" ]
        , br [] []
        , input
            [ placeholder "How are you feeling today?"
            , value ( .status ( getEmotionalCheckIn "k" model ) )
            , onInput ( SetEmotionalCheckInStatus "k" )
            ] []
        , br [] []
        -- TODO: Process the last updated timestamp as a user-friendly string
        , span [] [ text "Last updated: ???"]
        , br [] []
        , br [] []
        , label [] [ text "Y's emotional availability" ]
        , br [] []
        , input
            [ placeholder "How are you feeling today?"
            , value ( .status ( getEmotionalCheckIn "y" model ) )
            , onInput ( SetEmotionalCheckInStatus "y" )
            ] []
        , br [] []
        , span [] [ text "Last updated: ???"]
        ]
