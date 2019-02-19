module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrame)
import Debug
import Dict exposing (Dict)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Posix)


type State
    = Floating
    | Fighting Int
    | Finished


type alias Creature =
    { x : Int
    , y : Int
    , vx : Int
    , vy : Int
    , r : Int
    , state : State
    , rank : Int
    , id : Int
    }


type alias Creatures =
    List Creature


type alias Model =
    { creatures : Creatures
    , finished : Creatures
    , fighters : Dict Int (List ( Creature, Creature ))
    , amount : Int
    , width : Int
    , height : Int
    , framesPerFight : Int
    , fightPulsator : Int
    , creatureRadius : Int
    , vectorMaxX : Int
    , vectorMaxY : Int
    , maxRank : Int
    }


type Winner
    = LeftCreature
    | RightCreature


type alias FightResult =
    { winner : Winner
    , vector : ( Int, Int )
    }


type Msg
    = Noop
    | Frame Int
    | InitCreatures (List Creature)
    | ResultsIn ( Int, List FightResult )


generateWinner : Random.Generator Winner
generateWinner =
    Random.uniform LeftCreature [ RightCreature ]


generateCoords : Int -> Int -> Int -> Int -> Random.Generator ( Int, Int )
generateCoords minX maxX minY maxY =
    Random.map2
        (\x y ->
            ( x, y )
        )
        (Random.int minX maxX)
        (Random.int minY maxY)


generateCreature : Model -> Random.Generator Creature
generateCreature { creatureRadius, width, height, vectorMaxX, vectorMaxY } =
    let
        leftLimit =
            creatureRadius + 1

        rightLimit =
            width - creatureRadius - 1

        upperLimit =
            creatureRadius + 1

        lowerLimit =
            height - creatureRadius - 1
    in
    Random.map2
        (\pos vec ->
            createCreature creatureRadius pos vec
        )
        (generateCoords leftLimit rightLimit upperLimit lowerLimit)
        (generateCoords -vectorMaxX vectorMaxX -vectorMaxY vectorMaxY)


generateCreatures : Model -> Random.Generator (List Creature)
generateCreatures model =
    Random.list model.amount (generateCreature model)


generateFightResults : Int -> Int -> Model -> Random.Generator ( Int, List FightResult )
generateFightResults time len { vectorMaxX, vectorMaxY } =
    Random.map
        (\list ->
            ( time, list )
        )
        (Random.list len
            (Random.map2
                (\winner coords ->
                    FightResult winner coords
                )
                generateWinner
                (generateCoords -vectorMaxX vectorMaxX -vectorMaxY vectorMaxY)
            )
        )


createCreature : Int -> ( Int, Int ) -> ( Int, Int ) -> Creature
createCreature radius ( x, y ) ( vx, vy ) =
    Creature x y vx vy radius Floating 0 0


initCreatures : Model -> ( Model, Cmd Msg )
initCreatures model =
    let
        msg =
            Random.generate InitCreatures (generateCreatures model)
    in
    ( model, msg )


init : () -> ( Model, Cmd Msg )
init _ =
    initCreatures
        { creatures = []
        , finished = []
        , amount = 75
        , width = 750
        , height = 750
        , fighters = Dict.empty
        , framesPerFight = 15
        , fightPulsator = 7
        , creatureRadius = 20
        , vectorMaxX = 3
        , vectorMaxY = 3
        , maxRank = 5
        }


distance : Creature -> Creature -> Int
distance c1 c2 =
    let
        k1 =
            c1.x - c2.x

        k2 =
            c1.y - c2.y
    in
    (round << sqrt << toFloat) (k1 ^ 2 + k2 ^ 2)


updateCreatures : Model -> Model
updateCreatures model =
    { model
        | creatures =
            List.map
                (\creature ->
                    case creature.state of
                        Finished ->
                            creature

                        Fighting _ ->
                            creature

                        Floating ->
                            if creature.rank == model.maxRank then
                                { creature
                                    | state = Finished
                                }

                            else
                                { creature
                                    | x = creature.x + creature.vx
                                    , y = creature.y + creature.vy
                                }
                )
                model.creatures
    }


isPotentialEnemy : Creature -> Creature -> Bool
isPotentialEnemy creature creature2 =
    not (creature.id == creature2.id)
        && (creature.rank == creature2.rank)
        && (distance creature2 creature <= creature.r * 2)


collideCreatures : Model -> Model
collideCreatures model =
    { model
        | creatures =
            List.map
                (\creature ->
                    case creature.state of
                        Finished ->
                            creature

                        Fighting frame ->
                            if frame < model.framesPerFight then
                                { creature | state = Fighting (frame + 1) }

                            else
                                { creature | state = Floating }

                        Floating ->
                            let
                                isFighting =
                                    List.any
                                        (isPotentialEnemy creature)
                                        model.creatures
                            in
                            if isFighting then
                                { creature | state = Fighting 0 }

                            else
                                creature
                )
                model.creatures
    }


bounceCreatures : Model -> Model
bounceCreatures model =
    { model
        | creatures =
            List.map
                (\creature ->
                    let
                        { x, y, vx, vy } =
                            creature

                        r =
                            model.creatureRadius

                        lx =
                            x - r

                        rx =
                            x + r

                        uy =
                            y - r

                        ly =
                            y + r
                    in
                    if lx <= 0 || rx >= model.width then
                        { creature | vx = -vx }

                    else if uy <= 0 || ly >= model.height then
                        { creature | vy = -vy }

                    else
                        creature
                )
                model.creatures
    }


findFighters : Int -> Model -> Model
findFighters time model =
    { model
        | fighters =
            Dict.insert
                time
                (List.foldl
                    (\creature enemies ->
                        case creature.state of
                            Finished ->
                                enemies

                            Floating ->
                                enemies

                            Fighting frame ->
                                if frame > 0 then
                                    enemies

                                else
                                    model.creatures
                                        |> List.filter (isPotentialEnemy creature)
                                        |> List.head
                                        |> Maybe.map
                                            (\creature2 ->
                                                ( creature, creature2 ) :: enemies
                                            )
                                        |> Maybe.withDefault enemies
                    )
                    []
                    model.creatures
                )
                model.fighters
    }


organiseFights : Int -> Model -> ( Model, Cmd Msg )
organiseFights time model =
    if Dict.isEmpty model.fighters then
        ( model, Cmd.none )

    else
        let
            len =
                Dict.get time model.fighters
                    |> Maybe.map (\list -> List.length list)
                    |> Maybe.withDefault 0

            msg =
                Random.generate ResultsIn (generateFightResults time len model)
        in
        ( model, msg )


updateFighters : ( Creature, Creature ) -> FightResult -> ( Creature, Creature )
updateFighters ( creature1, creature2 ) { winner, vector } =
    let
        x =
            Tuple.first vector

        y =
            Tuple.second vector
    in
    case winner of
        LeftCreature ->
            ( { creature1
                | rank = creature1.rank + 1
                , vx = x
                , vy = y
              }
            , { creature2
                | rank =
                    if creature2.rank == 0 then
                        0

                    else
                        creature2.rank - 1
              }
            )

        RightCreature ->
            ( { creature1
                | rank =
                    if creature1.rank == 0 then
                        0

                    else
                        creature1.rank - 1
              }
            , { creature2
                | rank = creature2.rank + 1
                , vx = x
                , vy = y
              }
            )


flattenTupleList : List ( a, a ) -> List a
flattenTupleList list =
    List.foldr
        (\v acc ->
            Tuple.first v :: Tuple.second v :: acc
        )
        []
        list


findFinished : Model -> Model
findFinished model =
    { model
        | creatures =
            List.filter
                (\creature ->
                    case creature.state of
                        Finished ->
                            False

                        _ ->
                            True
                )
                model.creatures
        , finished =
            model.finished
                ++ List.filter
                    (\creature ->
                        case creature.state of
                            Finished ->
                                True

                            _ ->
                                False
                    )
                    model.creatures
    }


updateFightResults : Int -> List FightResult -> Model -> Model
updateFightResults time results model =
    let
        fighters =
            model.fighters
                |> Dict.get time
                |> Maybe.map
                    (\list ->
                        List.map2
                            updateFighters
                            list
                            results
                    )
                |> Maybe.withDefault []
                |> flattenTupleList
    in
    { model
        | creatures =
            List.map
                (\creature ->
                    List.foldr
                        (\fighter selected ->
                            if fighter.id == creature.id then
                                fighter

                            else
                                selected
                        )
                        creature
                        fighters
                )
                model.creatures
        , fighters = Dict.remove time model.fighters
    }


leftOf : b -> a -> ( a, b )
leftOf r l =
    ( l, r )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitCreatures list ->
            let
                withId =
                    List.map2
                        (\creature i ->
                            { creature | id = i }
                        )
                        list
                        (List.range 0 (List.length list))
            in
            { model | creatures = withId }
                |> leftOf Cmd.none

        Frame time ->
            model
                |> bounceCreatures
                |> collideCreatures
                |> findFinished
                |> findFighters time
                |> updateCreatures
                |> organiseFights time

        ResultsIn ( time, results ) ->
            model
                |> updateFightResults time results
                |> leftOf Cmd.none

        Noop ->
            ( model, Cmd.none )


svgAttributes : Model -> List (Attribute Msg)
svgAttributes model =
    let
        strWidth =
            String.fromInt model.width

        strHeight =
            String.fromInt model.height
    in
    [ width strWidth
    , height strHeight
    , viewBox <| "0 0 " ++ strWidth ++ " " ++ strHeight
    ]


colorByRank : Int -> String
colorByRank rank =
    let
        colors =
            Array.fromList
                [ "#000000"
                , "#444444"
                , "#888888"
                , "#BBBBBB"
                , "#FFFFFF"
                ]
    in
    Array.get rank colors
        |> Maybe.withDefault "#990099"


view : Model -> Svg Msg
view model =
    Svg.svg
        (svgAttributes model)
        (rect
            [ x "0"
            , y "0"
            , width "100%"
            , height "100%"
            , fill "#DD6633"
            ]
            []
            :: List.map
                (\creature ->
                    case creature.state of
                        Floating ->
                            circle
                                [ cx <| String.fromInt creature.x
                                , cy <| String.fromInt creature.y
                                , r <| String.fromInt creature.r
                                , fill <| colorByRank creature.rank
                                , stroke "#000"
                                ]
                                [ text "A" ]

                        Finished ->
                            circle
                                [ cx <| String.fromInt creature.x
                                , cy <| String.fromInt creature.y
                                , r <| String.fromInt creature.r
                                , fill "#00FF00"
                                , stroke "#000"
                                ]
                                [ text "W" ]

                        Fighting frame ->
                            let
                                radius =
                                    String.fromInt <| creature.r + modBy model.fightPulsator frame
                            in
                            circle
                                [ cx <| String.fromInt creature.x
                                , cy <| String.fromInt creature.y
                                , r radius
                                , fill "#FF0000"
                                , stroke "#000"
                                ]
                                [ text "F" ]
                )
                (model.finished ++ model.creatures)
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrame <|
            \posix ->
                Frame <| Time.posixToMillis posix
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
