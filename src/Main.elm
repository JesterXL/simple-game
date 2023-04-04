module Main exposing (main)

import Browser
import Html exposing (Html, button, div, img)
import Html.Events exposing (onClick, onMouseDown, onMouseUp )
import Html.Attributes exposing (src, style, class)
import Random
import Animator
import Animator.Inline
import Time
import Animator.Css
import Color
import Html.Lazy
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)

type alias Model =
    { paused : Bool
    , gravity : Float
    , frameNo : Int
    , mySquare : Component
    , walls : List Component
    , wallTimePassed : Float
    , initialSeed : Random.Seed
    , currentSeed : Random.Seed
    , gameOver : Bool }

initialModel : Random.Seed -> Model
initialModel seed =
    { paused = False
    , gravity = 0.05
    , frameNo = 0
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
        , gravitySpeed = 0 }
    , walls = []
    , wallTimePassed = 9001.0
    , initialSeed = seed
    , currentSeed = seed
    , gameOver = False }

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
    , gravitySpeed : Float}

type Msg
    = TogglePause
    | Frame Float
    | Accelerate Float

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none)
        Frame timePassed ->
            let
                updatedFrameNo = 
                    model.frameNo + 1
                updatedWallTimePassed =
                    model.wallTimePassed + timePassed
                (randomNum, newSeed) =
                    Random.step (Random.float 0.0 1.0) model.currentSeed
                wallsHit =
                    List.filter
                        (\wall -> testCrash model.mySquare wall)
                        model.walls
                    |> List.length
                hitAnyWalls =
                    wallsHit > 0
            in
            if hitAnyWalls == True then
                ( { model | 
                    gameOver = True
                    , paused = True
                    , frameNo = updatedFrameNo
                    , wallTimePassed = updatedWallTimePassed
                    , mySquare = updateSquarePosition model.mySquare
                    , walls = updateWalls randomNum False model.walls
                    , currentSeed = newSeed }
                , Cmd.none )
            else if updatedWallTimePassed < 2500 then
                ( { model | 
                    frameNo = updatedFrameNo
                    , wallTimePassed = updatedWallTimePassed
                    , mySquare = updateSquarePosition model.mySquare
                    , walls = updateWalls randomNum False model.walls
                    , currentSeed = newSeed }
                , Cmd.none )
            else
                ( { model | 
                    frameNo = updatedFrameNo
                    , wallTimePassed = 0
                    , mySquare = updateSquarePosition model.mySquare
                    , walls = updateWalls randomNum True model.walls
                    , currentSeed = newSeed }
                , Cmd.none )
        Accelerate amount ->
            if amount > 0 then
                ( { model | mySquare = updateSquareGravity model.mySquare amount |> resetSquareGravitySpeed }, Cmd.none )
            else
                ( { model | mySquare = updateSquareGravity model.mySquare amount }, Cmd.none )


updateWalls : Float -> Bool -> List Component -> List Component
updateWalls randomNum makeNewWall walls =
    if makeNewWall then
        let
            x = canvasWidth
            minHeight = 20
            maxHeight = 200
            height = randomNum * (maxHeight - minHeight + 1) + minHeight
            minGap = 50
            maxGap = 200
            gap = randomNum * (maxGap - minGap + 1) + minGap
            topWall =
                { type_ = "wall"
                , score = 0
                , width = 10
                , height = height
                , speedX = -1
                , speedY = 0
                , x = toFloat x
                , y = 0.0
                , gravity = 0
                , gravitySpeed = 0 }
            bottomWall =
                { type_ = "wall"
                , score = 0
                , width = 10
                , height = x - height - gap
                , speedX = -1
                , speedY = 0
                , x = toFloat x
                , y = height + gap
                , gravity = 0
                , gravitySpeed = 0 }
        in
        walls ++ [topWall, bottomWall]
    else
        walls
        |> List.map (\wall -> { wall | x = wall.x - 1 })
        |> List.filter (\wall -> wall.x + wall.width > 0)



updateSquareGravity : Component -> Float -> Component
updateSquareGravity square gravity =
    { square | gravity = gravity }

resetSquareGravitySpeed : Component -> Component
resetSquareGravitySpeed square =
    { square | gravitySpeed = -1.5}

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

testCrash : Component -> Component -> Bool
testCrash square wall =
    square.x < wall.x + wall.width &&
    square.x + square.width > wall.x &&
    square.y < wall.y + wall.height &&
    square.y + square.height > wall.y

view : Model -> Html Msg
view model =
    div [][
        div [][Html.text ("Score:" ++ (String.fromInt model.frameNo))]
        , div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            ]
            (
                [ Canvas.toHtml
                    ( canvasWidth, canvasHeight )
                    [ style "border" "1px solid rgba(0,0,0,0.1)" ]
                    (
                        [ clearScreen
                        , componentToRect model.mySquare ]
                        ++ (drawWalls model.walls)
                    )
                ] ++
                (
                    if model.gameOver == False then
                        [ button [ onClick TogglePause ] [ Html.text "Pause" ]
                        , button [ onMouseDown (Accelerate -0.2), onMouseUp (Accelerate 0.05) ] [ Html.text "Accelerate" ] 
                        ]
                    else
                        [ div [][Html.text "Game Over"]]
                )
            )
    ]

componentToRect component =
    shapes
        [ fill (Color.hsl 0.5 0.3 0.7) ]
        [ rect ( component.x, component.y ) component.width component.height ]

drawWalls walls =
    walls
    |> List.map wallToRect

wallToRect wall =
    shapes
        [ fill (Color.rgb255 0 0 1) ]
        [ rect ( wall.x, wall.y ) wall.width wall.height ]

canvasWidth =
    480

canvasHeight =
    270

centerX =
    canvasWidth / 2


centerY =
    canvasHeight / 2

clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) canvasWidth canvasHeight ]

newPos : Component -> Component
newPos component =
    { component 
        | gravitySpeed = component.gravitySpeed + component.gravity 
        , x = component.x + component.speedX 
        , y = component.y + component.speedY + component.gravitySpeed }

hitBottom : Component -> Bool
hitBottom component =
    let
        rockBottom =
            canvasHeight - component.height
    in
    if component.y > canvasHeight - component.height then
        True
    else
        False

collides : Component -> Component -> Bool
collides component otherComponent =
    let
        bottom =
            component.y + component.height
        top =
            component.y
        left =
            component.x
        right =
            component.x + component.width

        otherTop =
            otherComponent.y
        otherBottom =
            otherComponent.y + otherComponent.height
        otherLeft =
            otherComponent.x
        otherRight =
            otherComponent.x + otherComponent.width
        
    in
    if ((bottom < otherTop) || (top > otherBottom) || (right < otherLeft) || (left > otherLeft)) then
        False
    else
        True


init : Int -> (Model, Cmd Msg)
init datRando =
    ( initialModel (Random.initialSeed datRando) , Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused == False then
        onAnimationFrameDelta Frame
    else
        Sub.none

main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
