module Util.Time exposing (..)

import Time


type Date
    = Date { year : Int, month : Time.Month, day : Int }


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


posixToDate : Time.Zone -> Time.Posix -> Date
posixToDate tz time =
    let
        year =
            Time.toYear tz time

        month =
            Time.toMonth tz time

        day =
            Time.toDay tz time
    in
    Date { year = year, month = month, day = day }


formatDate : Date -> String
formatDate (Date date) =
    let
        year =
            String.fromInt date.year

        month =
            monthToString date.month

        day =
            String.fromInt date.day |> String.padLeft 2 '0'
    in
    year ++ " " ++ month ++ " " ++ day


formatTime : Time.Zone -> Time.Posix -> String
formatTime tz time =
    let
        date =
            posixToDate tz time

        hour =
            Time.toHour tz time |> String.fromInt |> String.padLeft 2 '0'

        minute =
            Time.toMinute tz time |> String.fromInt |> String.padLeft 2 '0'
    in
    formatDate date ++ " " ++ hour ++ ":" ++ minute


type alias Duration =
    { seconds : Int
    , minutes : Int
    , hours : Int
    , days : Int
    }


{-| Calculates the amount of time that passed between two dates.

The first date (t1) must be **before** the second date (t2), if this not the case, the function should return `Nothing`.

Relevant library functions:

  - Use Time.posixToMillis

```
import Time

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (1000)) --> Just (Duration 1 0 0 0)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (60 * 1000)) --> Just (Duration 0 1 0 0)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (60 * 60 * 1000)) --> Just (Duration 0 0 1 0)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (24 * 60 * 60 * 1000)) --> Just (Duration 0 0 0 1)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (24 * 60 * 60 * 1000 + 1000)) --> Just (Duration 1 0 0 1)

durationBetween (Time.millisToPosix 0) (Time.millisToPosix (4 * 24 * 60 * 60 * 1000 + 3 * 60 * 60 * 1000 + 2 * 60 * 1000 + 1000)) --> Just (Duration 1 2 3 4)

durationBetween (Time.millisToPosix 1000) (Time.millisToPosix 0) --> Nothing

durationBetween (Time.millisToPosix 1000) (Time.millisToPosix 1000) --> Nothing
```

-}
durationBetween : Time.Posix -> Time.Posix -> Maybe Duration
durationBetween timp1 timp2 =
    let
        milisecundeTimp1 = Time.posixToMillis timp1
        milisecundeTimp2 = Time.posixToMillis timp2
        diferentaMilisecunde = milisecundeTimp2 - milisecundeTimp1
    in
    if diferentaMilisecunde < 0 then
        Nothing
    else
        let
            totalSecunde = diferentaMilisecunde//1000
            secundeRamase = modBy 60 totalSecunde
            totalMinute = totalSecunde//60
            minuteRamase = modBy 60 totalMinute
            totalOre = totalMinute//60
            oreRamase = modBy 24 totalOre
            zile = totalOre // 24
        in
        Just { seconds = secundeRamase
             , minutes = minuteRamase
             , hours = oreRamase
             , days = zile }



{-| Format a `Duration` as a human readable string

    formatDuration (Duration 1 0 0 0) --> "1 second ago"

    formatDuration (Duration 2 0 0 0) --> "2 seconds ago"

    formatDuration (Duration 0 1 0 0) --> "1 minute ago"

    formatDuration (Duration 0 0 2 0) --> "2 hours ago"

    formatDuration (Duration 0 0 0 3) --> "3 days ago"

    formatDuration (Duration 0 1 1 1) --> "1 day 1 hour 1 minute ago"

    formatDuration (Duration 0 47 6 2) --> "2 days 6 hours 47 minutes ago"

    formatDuration (Duration 0 30 0 1) --> "1 day 30 minutes ago"

-}

formatDuration : Duration -> String
formatDuration timp =
    String.join " "
        (List.filter (\x -> x /= "")
            [ if timp.days > 0 then
                String.fromInt timp.days ++ 
                (case timp.days of
                    1 -> " day"
                    _ -> " days"
                )
              else
                ""
            , if timp.hours > 0 then
                String.fromInt timp.hours ++ 
                (case timp.hours of
                    1 -> " hour"
                    _ -> " hours"
                )
              else
                ""
            , if timp.minutes > 0 then
                String.fromInt timp.minutes ++ 
                (case timp.minutes of
                    1 -> " minute"
                    _ -> " minutes"
                )
              else
                ""
            , if timp.seconds > 0 then
                String.fromInt timp.seconds ++ 
                (case timp.seconds of
                    1 -> " second"
                    _ -> " seconds"
                )
              else
                ""
            ]
        ) ++
        if timp.days == 0 && timp.hours == 0 && timp.minutes == 0 && timp.seconds == 0 then
            ""
        else
            " ago"
