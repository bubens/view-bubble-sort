module Main exposing (Model, Msg, Status, bubbleSort, getColor, init, main, subscriptions, update, view, viewAllTheLittleBoxes)

import Browser
import Browser.Events as Browser
import Html exposing (Html)
import List
import List.Extra as List
import Random
import Random.List as Random
import Svg exposing (Svg)
import Svg.Attributes as Attributes


type Status
    = Unsorted
    | Sorted


type alias Model =
    { list : List (List Int)
    , status : Status
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        list =
            List.range 0 100
    in
    ( Model [ list ] Unsorted, Random.generate InitModel <| Random.shuffle list )


type Msg
    = InitModel (List Int)
    | NextPass


bubbleSort : List Int -> List Int
bubbleSort list =
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
                y :: bubbleSort (x :: xs)

            else
                x :: bubbleSort (y :: xs)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitModel ls ->
            ( { model | list = [ ls ], status = Unsorted }, Cmd.none )

        NextPass ->
            let
                last =
                    List.last model.list
                        |> Maybe.withDefault [ 0 ]

                next =
                    bubbleSort last
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


getColor : Int -> String
getColor x =
    let
        sx =
            toFloat x
                |> (*) (255 / 100)
                |> round
                |> String.fromInt
    in
    "rgb("
        ++ sx
        ++ ", "
        ++ sx
        ++ ", "
        ++ sx
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
                        , Attributes.fill <| getColor v
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
