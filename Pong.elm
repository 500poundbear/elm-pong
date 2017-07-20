module Main exposing (..)

import Html exposing (Html, div, text, program)
import Mouse
import Keyboard
import Time exposing (..)

import Collage exposing (..)
import Text exposing (..)
import Element exposing (..)
import Color
import List

import AnimationFrame exposing (..)

import Debug exposing (..)

-- MODEL

type alias Object =
    { width : Int
    , height : Int
    , x : Int
    , y : Int
    , color : Color.Color
    }

type alias ScoreBoard =
    { player: Int
    , enemy: Int
    }

type alias Boundary =
    { top : Int
    , left: Int
    , right: Int
    , bottom: Int
    }

type alias Displacement =
    { changeX : Int
    , changeY : Int
    }

type alias Game =
    { paddle : Object
    , enemy : Object
    , ball : Object
    , canvasHeight : Int
    , canvasWidth : Int
    , boundary : Boundary
    , ballDisplacement : Displacement
    , scoreBoard: ScoreBoard
    }


canvasBoundary =
    { top = 180
    , left = -380
    , right = 380
    , bottom = -180
    }

initialState =
    { paddle =
        { width = 10
        , height = 50
        , x = -380
        , y = 0
        , color = Color.blue
        }
    , enemy =
        { width = 10
        , height = 50
        , x = 380
        , y = 0
        , color = Color.red
        }
    , ball =
        { width = 10
        , height = 10
        , x = 150
        , y = 0
        , color = Color.brown
        }
    , canvasHeight = 400
    , canvasWidth = 800
    , boundary = canvasBoundary
    , ballDisplacement =
        { changeX = 3
        , changeY = 4
        }
    , scoreBoard =
        { player = 0
        , enemy = 0
        }
    }
init : ( Game, Cmd Msg )
init =
    ( initialState, Cmd.none )

-- MESSAGES
type Msg
    = KeyMsg Keyboard.KeyCode
    | Tick Time

-- VIEW
view : Game -> Html Msg
view game =
    toHtml <| collage (.canvasWidth game) (.canvasHeight game)
        [ toForm <| .paddle game
        , toForm <| .ball game
        , toForm <| .enemy game
        , toDisplayScore <| .scoreBoard game
        ]

moveObject : Object -> Int -> Boundary -> Int -> Object
moveObject object key boundary speed =
    let
        displacement = case key of
            38 ->
                speed
            40 ->
                speed * -1
            _ ->
                0
        offset = if ((displacement > 0 && object.y >= boundary.top) ||
            (displacement < 0 && object.y <= boundary.bottom)) then
                0
            else
                displacement
    in
    { object |
        y = object.y + offset
    }

moveEnemy : Object -> Object -> Boundary -> Object
moveEnemy enemy ball boundary =
    let
        enemyTop = toFloat enemy.y + (toFloat enemy.height / 2)
        enemyBottom = toFloat enemy.y - (toFloat enemy.height / 2)
        key =
            if toFloat ball.y > enemyTop then
                38
            else if toFloat ball.y < enemyBottom then
                40
            else 0
    in
    moveObject enemy key boundary 15

updateBall : Object -> Displacement -> Object
updateBall ball displacement =
    { ball
        | y = ball.y + displacement.changeY
        , x = ball.x + displacement.changeX
    }

{- Contains code that changes based on boundary -}
updateDisplacement : Object -> Boundary -> List Object -> Displacement -> Displacement
updateDisplacement object boundary objects displacement =
    let

        boundVertical = (object.y >= boundary.top || object.y <= boundary.bottom)

        {- Assumption is that only the y direction will change -}
        collide : Object -> Bool -> Bool
        collide obj b =
            let
                objectLeft = toFloat object.x - (toFloat object.width/2)
                objectRight = toFloat object.x + (toFloat object.width/2)

                objectTop = toFloat object.y + (toFloat object.height/2)
                objectBottom = toFloat object.y - (toFloat object.height/2)

                objLeft = toFloat obj.x - (toFloat obj.width/2)
                objRight = toFloat obj.x + (toFloat obj.width/2)

                objTop = toFloat obj.y + (toFloat obj.height/2)
                objBottom = toFloat obj.y - (toFloat obj.height/2)

                touches =
                    if (objectLeft <= objRight &&
                        objLeft <= objectRight &&
                        objectTop >= objBottom &&
                        objectBottom <= objTop
                    ) then
                        True
                    else
                        False
            in
            touches || b

        collideY = List.foldr collide False objects

        flipY = boundVertical
        flipX = collideY

        newY =
            if flipY then
                displacement.changeY * -1
            else
                displacement.changeY

        newX =
            if flipX then
                displacement.changeX * -1
            else
                displacement.changeX
    in
    { displacement
        | changeX = newX
        , changeY = newY
    }

resetIfOff : Game -> Game
resetIfOff game =
    let
        canvasLeft = 0 - (toFloat game.canvasWidth) / 2
        canvasRight = 0 + (toFloat game.canvasWidth) / 2
        gameball = game.ball
        resetBall =
            {gameball
            | x = 0
            , y = 0}
        scoreBoard = game.scoreBoard
        playerWins = {
            scoreBoard
            | enemy = scoreBoard.enemy + 1
        }
        enemyWins = {
            scoreBoard
            | player = scoreBoard.player + 1
        }
    in
    if toFloat game.ball.x < canvasLeft then
        {game
        | ball = resetBall
        , scoreBoard = playerWins
        }
    else if toFloat game.ball.x > canvasRight then
        {game
        | ball = resetBall
        , scoreBoard = enemyWins
        }
    else
        game

-- UPDATE
update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        KeyMsg code ->
            ( { game
                | paddle = moveObject game.paddle code game.boundary 25
              }
            ,
            Cmd.none )
        Tick time ->
            let
                disp = updateDisplacement game.ball game.boundary [game.paddle, game.enemy] game.ballDisplacement
                updatedGame = { game
                    | ballDisplacement = disp
                    , ball = updateBall game.ball disp
                    , enemy = moveEnemy game.enemy game.ball game.boundary
                }
            in
            ( resetIfOff updatedGame, Cmd.none )

-- SUBSCRIPTIONS


subscriptions : Game -> Sub Msg
subscriptions game =
    Sub.batch
        [ Keyboard.downs KeyMsg
        , times Tick]

-- MAIN

toDisplayScore : ScoreBoard -> Form
toDisplayScore sb =
    Collage.toForm <| centered <| concat <| List.map Text.fromString [toString sb.player, "     :     ", toString sb.enemy]

toForm : Object -> Form
toForm object =
    let shape = rect (toFloat <| .width object) (toFloat <| .height object)
        border = outlined (solid object.color) shape
        toF = toFloat
    in move (toF object.x, toF object.y) <| group [filled object.color shape, border]

main : Program Never Game Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
