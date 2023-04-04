module Main exposing (main)

import Browser
import Browser.Events exposing (Visibility(..), onAnimationFrameDelta, onVisibilityChange)
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (Html, button, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Random


type alias Model =
    { paused : Bool
    , gravity : Float
    , frame : Int
    , mySquare : Component
    , walls : List Component
    , wallTimePassed : Float
    , initialSeed : Random.Seed
    , currentSeed : Random.Seed
    , gameOver : Bool
    , screenHidden : Bool
    }


initialModel : Random.Seed -> Model
initialModel seed =
    { paused = False
    , gravity = 0.05
    , frame = 0
    , mySquare =
        { type_ = ""
        , score = 0
        , width = 30
        , height = 30
        , speedX = 0
        , speedY = 0
        , x = 10
        , y = 120
        , gravity = 0.05
        , gravitySpeed = 0
        }
    , walls = []
    , wallTimePassed = 9001.0
    , initialSeed = seed
    , currentSeed = seed
    , gameOver = False
    , screenHidden = False
    }


type alias Component =
    { type_ : String
    , score : Int
    , width : Float
    , height : Float
    , speedX : Float
    , speedY : Float
    , x : Float
    , y : Float
    , gravity : Float
    , gravitySpeed : Float
    }


type Msg
    = TogglePause -- pause and unpause the game
    | Frame Float -- the browser ticks; sends this every time it's ready to update an animation frame
    | Accelerate Float -- accelerate the square up or down
    | VisibilityChange Visibility -- when the browser tab is hidden or shown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none )

        Frame timePassed ->
            let
                updatedframe =
                    model.frame + 1

                updatedWallTimePassed =
                    model.wallTimePassed + timePassed

                ( randomNum, newSeed ) =
                    Random.step (Random.float 0.0 1.0) model.currentSeed

                hitAnyWalls =
                    List.filter
                        (\wall -> testCrash model.mySquare wall)
                        model.walls
                        |> List.length
                        |> (\length -> length > 0)
            in
            if hitAnyWalls == True then
                ( { model
                    | gameOver = True
                    , paused = True
                    , frame = updatedframe
                    , wallTimePassed = updatedWallTimePassed
                    , mySquare = updateSquarePosition model.mySquare
                    , walls = updateWalls randomNum False model.walls
                    , currentSeed = newSeed
                  }
                , Cmd.none
                )

            else if updatedWallTimePassed < newWallDrawTime then
                ( { model
                    | frame = updatedframe
                    , wallTimePassed = updatedWallTimePassed
                    , mySquare = updateSquarePosition model.mySquare
                    , walls = updateWalls randomNum False model.walls
                    , currentSeed = newSeed
                  }
                , Cmd.none
                )

            else
                ( { model
                    | frame = updatedframe
                    , wallTimePassed = 0
                    , mySquare = updateSquarePosition model.mySquare
                    , walls = updateWalls randomNum True model.walls
                    , currentSeed = newSeed
                  }
                , Cmd.none
                )

        Accelerate amount ->
            if amount > 0 then
                -- if you are going up, update your speed so it slowly floats up and stops,
                -- otherwise you'll just keep floating up. If I increase gravity too much, you fall too fast.
                ( { model | mySquare = updateSquareGravity model.mySquare amount |> resetSquareGravitySpeed }, Cmd.none )

            else
                ( { model | mySquare = updateSquareGravity model.mySquare amount }, Cmd.none )

        VisibilityChange vis ->
            case vis of
                Hidden ->
                    ( { model | paused = True, screenHidden = True }, Cmd.none )

                Visible ->
                    ( { model | paused = False, screenHidden = False }, Cmd.none )


updateWalls : Float -> Bool -> List Component -> List Component
updateWalls randomNum makeNewWall walls =
    if makeNewWall then
        let
            minHeight =
                20

            maxHeight =
                200

            height =
                randomNum * (maxHeight - minHeight + 1) + minHeight

            minGap =
                50

            maxGap =
                200

            gap =
                randomNum * (maxGap - minGap + 1) + minGap

            topWall =
                { type_ = "wall"
                , score = 0
                , width = 10
                , height = height
                , speedX = -1
                , speedY = 0
                , x = canvasWidth
                , y = 0.0
                , gravity = 0
                , gravitySpeed = 0
                }

            bottomWall =
                { type_ = "wall"
                , score = 0
                , width = 10
                , height = canvasWidth - height - gap
                , speedX = -1
                , speedY = 0
                , x = canvasWidth
                , y = height + gap
                , gravity = 0
                , gravitySpeed = 0
                }
        in
        walls ++ [ topWall, bottomWall ]

    else
        -- move all walls left, then
        -- remove walls that are off the screen
        walls
            |> List.map (\wall -> { wall | x = wall.x - 1 })
            |> List.filter (\wall -> wall.x + wall.width > 0)


updateSquareGravity : Component -> Float -> Component
updateSquareGravity square gravity =
    { square | gravity = gravity }


resetSquareGravitySpeed : Component -> Component
resetSquareGravitySpeed square =
    { square | gravitySpeed = -1.5 }


updateSquarePosition : Component -> Component
updateSquarePosition square =
    let
        updatedSquare =
            newPos square
    in
    if square.gravity > 0 && hitBottom square then
        { updatedSquare | gravitySpeed = 0, y = canvasHeight - square.height }

    else
        updatedSquare



-- Copilot wrote this function and I hate how it looks


testCrash : Component -> Component -> Bool
testCrash square wall =
    square.x
        < wall.x
        + wall.width
        && square.x
        + square.width
        > wall.x
        && square.y
        < wall.y
        + wall.height
        && square.y
        + square.height
        > wall.y


view : Model -> Html Msg
view model =
    div []
        [ div [] [ Html.text ("Score:" ++ String.fromInt model.frame) ]
        , div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            ]
            [ Canvas.toHtml
                ( round canvasWidth, round canvasHeight )
                [ style "border" "1px solid rgba(0,0,0,0.1)" ]
                (clearScreen
                    :: componentToRect model.mySquare
                    :: drawWalls model.walls
                )
            ]
        , div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "padding-top" "8px"
            ]
            (viewGameButtons model)
        ]


viewGameButtons : Model -> List (Html Msg)
viewGameButtons model =
    if model.gameOver == False then
        [ if model.paused == True then
            button [ onClick TogglePause ] [ Html.text "Unpause" ]

          else
            button [ onClick TogglePause ] [ Html.text "Pause" ]
        , button [ onMouseDown (Accelerate -0.2), onMouseUp (Accelerate 0.05) ] [ Html.text "Accelerate" ]
        ]

    else
        [ div [] [ Html.text "Game Over" ] ]


componentToRect : Component -> Canvas.Renderable
componentToRect component =
    shapes
        [ fill (Color.hsl 0.5 0.3 0.7) ]
        [ rect ( component.x, component.y ) component.width component.height ]


drawWalls : List Component -> List Canvas.Renderable
drawWalls walls =
    walls
        |> List.map wallToRect


wallToRect : Component -> Canvas.Renderable
wallToRect wall =
    shapes
        [ fill (Color.rgb255 0 0 1) ]
        [ rect ( wall.x, wall.y ) wall.width wall.height ]


canvasWidth : Float
canvasWidth =
    480


canvasHeight : Float
canvasHeight =
    270


newWallDrawTime : Float
newWallDrawTime =
    2500


clearScreen : Canvas.Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) canvasWidth canvasHeight ]


newPos : Component -> Component
newPos component =
    { component
        | gravitySpeed = component.gravitySpeed + component.gravity
        , x = component.x + component.speedX
        , y = component.y + component.speedY + component.gravitySpeed
    }


hitBottom : Component -> Bool
hitBottom component =
    if component.y > canvasHeight - component.height then
        True

    else
        False



-- For now, we pass in Math.random() from JavaScript. At some point,
-- we should get a better random number generator.


init : Int -> ( Model, Cmd Msg )
init datRando =
    ( initialModel (Random.initialSeed datRando), Cmd.none )



-- if paused, stop sending animation frame updates


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameOver == True then
        Sub.none

    else if model.paused == False then
        Sub.batch [ onVisibilityChange VisibilityChange, onAnimationFrameDelta Frame ]

    else
        onVisibilityChange VisibilityChange


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
