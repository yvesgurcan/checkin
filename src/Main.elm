
module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Styled.Attributes exposing (css)
import List exposing (..)
import Date exposing (..)
import Css exposing (..)
import Task exposing (..)
import Time exposing (Posix, millisToPosix, now, toYear, toMonth, toDay, utc, Month(..))

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = 
    { now: Posix
    , emotionalCheckIns: List EmotionalCheckIn
    }

type alias EmotionalCheckIn =
    { id: String
    , status : String
    , lastUpdated : Posix
    }


    
init : () -> ( Model, Cmd UpdatePayload )
init flags =
    ({ now = millisToPosix 0
       , emotionalCheckIns =
            [ { id = "k", status = ":)", lastUpdated = millisToPosix 0 }
              , { id = "y", status = ":D", lastUpdated = millisToPosix 0 }
            ]
    }
    , perform UpdateCurrentTime now
    )

type UpdatePayload
    = NoOp
    | SetEmotionalCheckInStatus String String
    | UpdateCurrentTime Posix

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
                        { emotionalCheckIn | lastUpdated = model.now , status = status }
                    else
                        emotionalCheckIn
                updatedEmotionalCheckIns = List.map updateEmotionalCheckIn model.emotionalCheckIns
            -- TODO: Change value of lastUpdated as well to be current timestamp.
            -- TODO: Send a POST request. Will need backend as well.
            in ({ model | emotionalCheckIns = updatedEmotionalCheckIns }, Cmd.none )

        UpdateCurrentTime time ->
            ({ model | now = time }, Cmd.none)




subscriptions : Model -> Sub UpdatePayload
subscriptions _ =
    Sub.none

getEmotionalCheckIn : String -> Model -> EmotionalCheckIn
getEmotionalCheckIn id model = 
    let
        filterEmotionalCheckIn emotionalCheckIn = emotionalCheckIn.id == id
        matchEmotionalCheckIn = head ( filter filterEmotionalCheckIn model.emotionalCheckIns )
    in case matchEmotionalCheckIn of
        Just val -> val
        -- TODO: Let the function return Nothing if the record was not found instead of returning a misleadaing ID.
        Nothing -> { id = "BOOM", status = "", lastUpdated = millisToPosix 0 }

getEmotionalCheckInStatus : String -> Model -> String
getEmotionalCheckInStatus  id model = ( .status ( getEmotionalCheckIn id model ) )

getEmotionalCheckInLastUpdated : String -> Model -> Posix
getEmotionalCheckInLastUpdated id model = ( .lastUpdated ( getEmotionalCheckIn id model ) )

getUserReadableValueFromPosix : Posix -> String
getUserReadableValueFromPosix timestamp =
    let userReadableValue = String.fromInt ( toYear utc timestamp ) ++ "-" ++ getMonth (toMonth utc timestamp ) ++ "-" ++ getDay ( toDay utc timestamp )
    in
        if (userReadableValue == "1970-01-01") then ""
        else userReadableValue

getMonth : Month -> String
getMonth month =
    case month of
        Jan -> "01"
        Feb -> "02"
        Mar -> "03"
        Apr -> "04"
        May -> "05"
        Jun -> "06"
        Jul -> "07"
        Aug -> "08"
        Sep -> "09"
        Oct -> "10"
        Nov -> "11"
        Dec -> "12"

getDay : Int -> String
getDay day =
    if day > 9 then String.fromInt day
    else "0" ++ String.fromInt day
    

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
            , value ( getEmotionalCheckInStatus "k" model )
            , onInput ( SetEmotionalCheckInStatus "k" )
            ] []
        , br [] []
        -- TODO: Process the last updated timestamp as a user-friendly string
        , span [] [ text ( "Last updated: " ++ getUserReadableValueFromPosix ( getEmotionalCheckInLastUpdated "k" model ) )]
        , br [] []
        , br [] []
        , label [] [ text "Y's emotional availability" ]
        , br [] []
        , input
            [ placeholder "How are you feeling today?"
            , value ( getEmotionalCheckInStatus "y" model )
            , onInput ( SetEmotionalCheckInStatus "y" )
            ] []
        , br [] []
        , span [] [ text ( "Last updated: " ++ getUserReadableValueFromPosix ( getEmotionalCheckInLastUpdated "y" model ) )]
        ]
