module Main exposing (main)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    { value : Int
    , base : String
    }


init : ( Model, Cmd Msg )
init =
    ( { value = 999
      , base = "01"
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateBase String
    | UpdateValue String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateBase newBase ->
            let
                base =
                    case String.length newBase <= 1 of
                        True ->
                            model.base

                        False ->
                            newBase
            in
            ( { model | base = base }, Cmd.none )

        UpdateValue strVal ->
            let
                val =
                    case String.toInt strVal of
                        Ok newVal ->
                            newVal

                        Err _ ->
                            model.value
            in
            ( { model | value = val }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [ class "code bg-navy white tracked w-100 vh-100 flex flex-column items-center justify-center" ]
        [ h2 [ class "f1 tracked" ] [ Html.text <| convertToBase model.value model.base ]
        , input
            [ toString model.value |> value
            , onInput UpdateValue
            , class "tracked ma3 w-100 white code mw6 tc pa3 f4 bg-transparent ba o b--white-40 br2"
            ]
            []
        , input
            [ model.base |> value
            , onInput UpdateBase
            , class "tracked ma3 w-100 white code mw6 tc pa3 f4 bg-transparent ba b--white-40 br2"
            ]
            []
        , div [ class "w-100 mw6 flex overflow-hidden" ] <|
            List.map
                (\( name, bases ) ->
                    button
                        [ onClick <| UpdateBase bases
                        , class "grow flex-auto ma1 pv2 br1 bg-transparent hover-bg-dark-blue white b--white-20"
                        ]
                        [ text name ]
                )
                basesList
        ]


basesList =
    [ ( "Binary", "01" )
    , ( "Octal", "01234567" )
    , ( "Decimal", "0123456789" )
    , ( "Duodecimal", "0123456789ab" )
    , ( "Hexadecimal", "0123456789abcdef" )
    ]


convertToBase : Int -> String -> String
convertToBase value baseDigits =
    baseDigits
        |> String.toList
        |> Array.fromList
        |> (\x -> toBaseAs x value)
        |> String.fromList


toBaseAs : Array a -> Int -> List a
toBaseAs terms x =
    toBase (Array.length terms) x
        |> mapAsIndices terms


mapAsIndices : Array a -> List Int -> List a
mapAsIndices source =
    List.filterMap (\i -> Array.get i source)


toBase : Int -> Int -> List Int
toBase =
    toBaseHelper []


toBaseHelper : List Int -> Int -> Int -> List Int
toBaseHelper acc base x =
    case divMod base x of
        Nothing ->
            acc

        Just ( 0, y ) ->
            y :: acc

        Just ( next, y ) ->
            toBaseHelper (y :: acc) base next


divMod : Int -> Int -> Maybe ( Int, Int )
divMod base x =
    if base == 0 then
        Nothing
    else
        Just ( x // base, x % base )
