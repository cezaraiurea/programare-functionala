module View.Posts exposing (..)

import Html exposing (Html, div, text, td, tr, th, table, thead, tbody, a, option, select,input)
import Html.Attributes exposing (href, class, type_, id, selected, value, checked)
import Html.Events exposing(onCheck, onInput)
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
headerFields : Html Msg
headerFields =
    tr []
        [ th [][text "Score"]
        , th [][text "Title"]
        , th [][text "Type"]
        , th [][text "Posted Date"]
        , th [][text "Link"]
        ]

creareRand : Post -> Time.Posix -> Html Msg
creareRand post currentTime =
    let
        durataPostarii = Util.Time.durationBetween post.time currentTime 
        textDurataRelativa =
            case durataPostarii of
                Nothing -> ""
                Just durataRelativa -> " (" ++ Util.Time.formatDuration durataRelativa ++ ")"
    in
    tr []
        [ td [class "post-score"][text (String.fromInt post.score)]
        , td [class "post-title"][text post.title ]
        , td [class "post-type" ][text post.type_ ]
        , td [class "post-time" ][text (Util.Time.formatTime Time.utc post.time ++ textDurataRelativa)]
        , td [class "post-url"]
             [ case post.url of
                Nothing -> text "No url"
                Just url -> a [href url][text "Url"]
             ]
        ]

postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable _ currentTime posts =
    table []
        [ thead [][headerFields]
        , tbody [] (List.map (\post -> creareRand post currentTime) posts)
        ]


{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
showPostsPerPage : Int -> Html Msg
showPostsPerPage nrPosts =
    div []
        [ select
            [ id "select-posts-per-page"
            , onInput (\value -> ConfigChanged (ShowPostsPerPage (String.toInt value |> Maybe.withDefault 10)))
            ]
            [ option [value "10", selected (nrPosts==10)][text "10"]
            , option [value "25", selected (nrPosts==25)][text "25"]
            , option [value "50", selected (nrPosts==50)][text "50"]
            ]
        ]

showSorted : SortBy -> Html Msg
showSorted criteriuSortare =
    div []
        [ select
            [ id "select-sort-by"
            , onInput (\value -> ConfigChanged (ShowSortBy (sortFromString value |> Maybe.withDefault None)))
            ]
            [ option [value "Score", selected (criteriuSortare==Score)][text "Score"]
            , option [value "Title", selected (criteriuSortare==Title)][text "Title"]
            , option [value "Posted", selected (criteriuSortare==Posted)][text "Posted"]
            , option [value "None", selected (criteriuSortare==None)][text "None"]
            ]
        ]

showJobs : Bool -> Html Msg
showJobs jobsShow =
    div []
        [ input
            [ type_ "checkbox"
            , id "checkbox-show-job-posts"
            , checked jobsShow
            , onCheck (\isChecked -> ConfigChanged (ShowJobs isChecked))
            ]
            []
        , text "Show job posts"
        ]

showPostsWithNoUrl : Bool -> Html Msg
showPostsWithNoUrl showTextOnly =
    div []
        [ input
            [ type_ "checkbox"
            , id "checkbox-show-text-only-posts"
            , checked showTextOnly
            , onCheck (\isChecked -> ConfigChanged (ShowNoUrl isChecked))
            ]
            []
        , text "Show text only posts"
        ]

postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    div []
        [ div []
            [ text "Posts per page: "
            , select
                [ id "select-posts-per-page"
                , onInput (\value -> ConfigChanged (ShowPostsPerPage (String.toInt value |> Maybe.withDefault 10)))
                ]
                [ option [value "10", selected (config.postsToShow==10)][text "10"]
                , option [value "25", selected (config.postsToShow==25)][text "25"]
                , option [value "50", selected (config.postsToShow==50)][text "50"]
                ]
            ]
        , div []
            [ text "Sort by: "
            , select
                [ id "select-sort-by"
                , onInput (\value -> ConfigChanged (ShowSortBy (sortFromString value |> Maybe.withDefault None)))
                ]
                [ option [value "Score", selected (config.sortBy==Score)][text "Score"]
                , option [value "Title", selected (config.sortBy==Title)][text "Title"]
                , option [value "Posted", selected (config.sortBy==Posted)][text "Posted"]
                , option [value "None", selected (config.sortBy==None)][text "None"]
                ]
            ]
        , div []
            [ input
                [ type_ "checkbox"
                , id "checkbox-show-job-posts"
                , checked config.showJobs
                , onCheck (\isChecked -> ConfigChanged (ShowJobs isChecked))
                ]
                []
            , text "Show job posts"
            ]
        , div []
            [ input
                [ type_ "checkbox"
                , id "checkbox-show-text-only-posts"
                , checked config.showTextOnly
                , onCheck (\isChecked -> ConfigChanged (ShowNoUrl isChecked))
                ]
                []
            , text "Show text-only posts"
            ]
        ]
