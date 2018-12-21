module Main exposing (Color, Model, Msg(..), generateInitList, init, main, update, view)

import Browser
import Browser.Events as Browser
import Html exposing (Html)
import List
import List.Extra as List
import Random
import Svg exposing (Svg)
import Svg.Attributes as Attributes


type Status
    = Unsorted
    | Sorted


type alias Model =
    { list : List (List Int)
    , colors : Colors
    , status : Status
    }


type alias Color =
    { r : Int
    , g : Int
    , b : Int
    }


type alias Colors =
    List Color


generateColor : Random.Generator Color
generateColor =
    Random.map3 Color (Random.int 0 255) (Random.int 0 255) (Random.int 0 255)


generateColors : Random.Generator Colors
generateColors =
    generateColor
        |> Random.list 10


generateInitList : Random.Generator (List Int)
generateInitList =
    Random.int 0 9
        |> Random.list 100


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ [ 0 ] ] [ Color 0 0 0 ] Unsorted, Random.generate InitModel generateInitList )


type Msg
    = InitModel (List Int)
    | InitColors Colors
    | NextPass


singlePassOfSort : List Int -> List Int
singlePassOfSort list =
    case list of
        [] ->
            []

        [ x ] ->
            [ x ]

        [ x, y ] ->
            if x > y then
                [ y, x ]

            else
                [ x, y ]

        x :: y :: xs ->
            if x > y then
                y :: singlePassOfSort (x :: xs)

            else
                x :: singlePassOfSort (y :: xs)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitModel ls ->
            ( { model | list = [ ls ], status = Unsorted }, Random.generate InitColors generateColors )

        InitColors colors ->
            ( { model | colors = colors }, Cmd.none )

        NextPass ->
            let
                last =
                    List.last model.list
                        |> Maybe.withDefault [ 0 ]

                next =
                    singlePassOfSort last
            in
            if not (last == next) then
                ( { model
                    | list =
                        List.reverse model.list
                            |> (::) next
                            |> List.reverse
                  }
                , Cmd.none
                )

            else
                ( { model | status = Sorted }, Cmd.none )


getColor : Int -> Colors -> String
getColor x colors =
    let
        clr =
            List.getAt x colors
                |> Maybe.withDefault (Color 0 0 0)
    in
    "rgb("
        ++ String.fromInt clr.r
        ++ ", "
        ++ String.fromInt clr.g
        ++ ", "
        ++ String.fromInt clr.b
        ++ ")"


viewAllTheLittleBoxes : Model -> List (Svg Msg)
viewAllTheLittleBoxes model =
    let
        viewRow : Int -> List Int -> List (Svg Msg)
        viewRow y list =
            List.indexedMap
                (\x v ->
                    let
                        x_ =
                            x * 10 |> String.fromInt

                        y_ =
                            y * 10 |> String.fromInt
                    in
                    Svg.rect
                        [ Attributes.x x_
                        , Attributes.y y_
                        , Attributes.width "10"
                        , Attributes.height "10"
                        , Attributes.fill <| getColor v model.colors
                        ]
                        []
                )
                list
    in
    model.list
        |> List.indexedMap viewRow
        |> List.concat


view : Model -> Html Msg
view model =
    let
        height =
            List.length model.list
                |> (*) 10
                |> String.fromInt

        width =
            List.foldl (\x _ -> List.length x) 0 model.list
                |> (*) 10
                |> String.fromInt
    in
    Svg.svg
        [ Attributes.width width
        , Attributes.height height
        , Attributes.viewBox ("0 0 " ++ width ++ " " ++ height)
        ]
        (viewAllTheLittleBoxes model)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Unsorted ->
            Browser.onAnimationFrame (\_ -> NextPass)

        Sorted ->
            Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
