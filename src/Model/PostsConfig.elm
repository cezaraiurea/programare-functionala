module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)

import Html.Attributes exposing (scope)
import Model.Post exposing (Post)
import Time


type SortBy
    = Score
    | Title
    | Posted
    | None


sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]


sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"

        Title ->
            "Title"

        Posted ->
            "Posted"

        None ->
            "None"


{-|

    sortFromString "Score" --> Just Score

    sortFromString "Invalid" --> Nothing

    sortFromString "Title" --> Just Title

-}
sortFromString : String -> Maybe SortBy
sortFromString string = 
 case string of
     "Score" -> Just Score
     "Title" -> Just Title
     "Posted" -> Just Posted
     "None" -> Just None 
     _ -> Nothing

sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score ->
            \postA postB -> compare postB.score postA.score

        Title ->
            \postA postB -> compare postA.title postB.title

        Posted ->
            \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ


type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }


defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True


{-| A type that describes what option changed and how
-} 
type Change
    =  ShowPostsPerPage Int 
    | ShowSortBy SortBy
    | ShowJobs Bool
    | ShowNoUrl Bool

{-| Given a change and the current configuration, return a new configuration with the changes applied
-}
applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges schimbare config =
    case schimbare of
        ShowPostsPerPage numar ->
            { config | postsToShow = numar }

        ShowSortBy criteriu ->
            { config | sortBy = criteriu }

        ShowJobs afiseazaJoburi ->
            { config | showJobs = afiseazaJoburi }

        ShowNoUrl afiseazaTextOnly ->
            { config | showTextOnly = afiseazaTextOnly }



{-| Given the configuration and a list of posts, return the relevant subset of posts according to the configuration

Relevant local functions:

  - sortToCompareFn

Relevant library functions:

  - List.sortWith

-}
filterPosts : PostsConfig -> List Post -> List Post
filterPosts config posts =
    posts
        |> List.filter (\post ->
            if config.showTextOnly then
                post.url/= Nothing
            else
                True
        )
        |> List.filter (\post ->
            if config.showJobs then
                True
            else
                post.type_ /="job"
        )
        |> List.sortWith (sortToCompareFn config.sortBy)  
        |> List.take config.postsToShow  
