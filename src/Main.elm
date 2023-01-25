
module Main exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Browser
import Css exposing (..)
import List exposing (..)
import Date exposing (..)
import Task exposing (..)
import Time exposing (Posix, millisToPosix, posixToMillis, now, toYear, toMonth, toDay, utc, Month(..))
import Duration exposing (..)

main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = 
    { now: Posix
    , emotionalCheckIns: List EmotionalCheckIn
    }

type alias EmotionalCheckIn =
    { id: String
    , name: String
    , status : String
    , lastUpdated : Posix
    }


    
init : () -> ( Model, Cmd UpdatePayload )
init flags =
    ({ now = millisToPosix 0
       , emotionalCheckIns =
            [ { id = "k", name = "K", status = ":)", lastUpdated = millisToPosix 0 }
              , { id = "y", name = "Y", status = ":D", lastUpdated = millisToPosix 0 }
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
    case
        payload of
        NoOp ->
            ( model, Cmd.none )

        SetEmotionalCheckInStatus id status ->
            let
                updateEmotionalCheckIn emotionalCheckIn =
                    if (emotionalCheckIn.id == id) then
                        { emotionalCheckIn | lastUpdated = model.now, status = status }
                    else
                        emotionalCheckIn
                updatedEmotionalCheckIns = List.map updateEmotionalCheckIn model.emotionalCheckIns
            -- TODO: Send a POST request. Will need backend as well.
            in ({ model | emotionalCheckIns = updatedEmotionalCheckIns }, Cmd.none )

        UpdateCurrentTime time ->
            ({ model | now = time }, Cmd.none)


subscriptions : Model -> Sub UpdatePayload
subscriptions model = Time.every 1000 UpdateCurrentTime

getHumanReadableDate : Posix -> String
getHumanReadableDate timestamp =
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

getDuration : Duration.Duration -> String
getDuration differenceFromNow = 
    let
        durationHelpers =
            if (inJulianYears differenceFromNow >= 1) then ({ converter = inJulianYears, unit = "year"})
            -- Elm doesn't come with an inMonths function, somehow ¯\_(ツ)_/¯
            else if (inWeeks differenceFromNow >= 1) then ({ converter = inWeeks, unit = "week"})
            else if (inDays differenceFromNow >= 1) then ({ converter = inDays, unit = "day"})
            else if ( ( inHours differenceFromNow ) >= 1) then ({ converter = inHours, unit = "hour"})
            else if (inMinutes differenceFromNow >= 1) then ({ converter = inMinutes, unit = "minute"})
            else ({ converter = inSeconds, unit = "second" })
    in
        if inSeconds differenceFromNow == 0 then "just now"
        else
            getHumanReadableDurationFragment ( Basics.round ( durationHelpers.converter differenceFromNow ) ) durationHelpers.unit ++ " ago."

getHumanReadableDurationFragment : Int -> String -> String
getHumanReadableDurationFragment duration unit =
    case
        duration of
        0 -> ""
        1 -> String.fromInt duration ++ " " ++ unit
        _ -> String.fromInt duration ++ " " ++ unit ++ "s"

getHumanReadableDuration : Posix -> Posix -> String
getHumanReadableDuration pastDate now =
    if ( posixToMillis pastDate == 0) then "-"
    else
        let
            differenceFromNow = milliseconds ( toFloat ( posixToMillis now - posixToMillis pastDate ) )
        in  getDuration differenceFromNow

viewGetCheckIns : Model -> Html UpdatePayload
viewGetCheckIns model =
    let
        viewGetCheckIn emotionalCheckIn =
            let
                _ = Debug.log emotionalCheckIn.id ( "status: " ++ emotionalCheckIn.status ++ ", lastUpdated: " ++ String.fromInt ( posixToMillis emotionalCheckIn.lastUpdated ) ++ ", now: " ++ String.fromInt ( posixToMillis model.now ) )
            in
            div []
                [ label [] [ text ( emotionalCheckIn.name ++ "'s emotional availability" ) ]
                , br [] []
                , input
                    [ placeholder "How are you feeling today?"
                    , value ( emotionalCheckIn.status )
                    , onInput ( SetEmotionalCheckInStatus emotionalCheckIn.id )
                    ] []
                , br [] []
                , span [] [ text ( "Last updated: " ++ ( getHumanReadableDuration emotionalCheckIn.lastUpdated model.now ) )]
                , br [] []
                , br [] []
                ]
    in div [] ( List.map viewGetCheckIn model.emotionalCheckIns )

view : Model -> Html UpdatePayload
view model =
    main_
        -- TODO: Style the view.
        [ css
            [ minHeight (vh 100)
            , padding (rem 3)
            , backgroundColor ( hex "000000" )
            , color ( hex "ffffff" )
            ]
        ]
        [ h1 [] [ text "Emotional availabilty check-in" ]
        , viewGetCheckIns model
        ]
