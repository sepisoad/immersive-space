module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Debug exposing (..)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (height, style, width)
import Html.Events as HE exposing (onClick)
import Http
import Json.Decode as JD
import Math.Vector3 as V3
import Model3D as M3D
import ObjectFileDecoder as Obj3D
import Utils as U
import WebGL as GL


type alias Model =
    { theta : Float
    , lightLocation : V3.Vec3
    , sphere : Obj3D.Mesh
    , cube : Obj3D.Mesh
    , pointer : { x : Float, y : Float }
    , size : { w : Int, h : Int }
    }


type Msg
    = Ticked
    | Sphere3DLoaded (Result Http.Error String)
    | Cube3DLoaded (Result Http.Error String)
    | PointerMoved Int Int


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


init : ( Model, Cmd Msg )
init =
    ( Model
        0.0
        (U.updateLightLocation 1.3 1.3 1)
        Obj3D.empty
        Obj3D.empty
        { x = 0, y = 0 }
        { w = 400, h = 400 }
    , Cmd.batch
        [ U.load3DObject "3d-models/sphere.txt" Sphere3DLoaded
        , U.load3DObject "3d-models/cube.txt" Cube3DLoaded
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrameDelta (\_ -> Ticked)


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Ticked ->
            let
                newTheta =
                    if (model.theta + 1) > 360 then
                        0

                    else
                        model.theta + 1

                ll =
                    U.updateLightLocation 1.3 1.3 1

                x =
                    V3.getX ll

                y =
                    V3.getY ll

                z =
                    V3.getZ ll

                newX =
                    x * cos (U.degToRad newTheta) - (y * sin (U.degToRad newTheta))

                newY =
                    y

                newZ =
                    z * sin (U.degToRad newTheta) + (y * cos (U.degToRad newTheta))

                lloc =
                    V3.vec3 newX newY newZ
            in
            ( { model | theta = newTheta, lightLocation = lloc }, Cmd.none )

        Sphere3DLoaded result ->
            case result of
                Ok object ->
                    let
                        obj3d =
                            Obj3D.load object
                    in
                    ( { model | sphere = obj3d }, Cmd.none )

                Err httpError ->
                    let
                        log_ =
                            log "" httpError
                    in
                    ( model, Cmd.none )

        Cube3DLoaded result ->
            case result of
                Ok object ->
                    let
                        obj3d =
                            Obj3D.load object
                    in
                    ( { model | cube = obj3d }, Cmd.none )

                Err httpError ->
                    let
                        log_ =
                            log "" httpError
                    in
                    ( model, Cmd.none )

        PointerMoved px py ->
            let
                pointer =
                    { x = (toFloat -px / toFloat model.size.w) + 0.5, y = (toFloat -py / toFloat model.size.h) + 0.5 }

                log_ =
                    log "[x,y]" pointer
            in
            ( { model | pointer = pointer }, Cmd.none )


onMouseMove : (Int -> Int -> Msg) -> Attribute Msg
onMouseMove msg =
    let
        decoder =
            JD.map2 msg
                (JD.field "offsetX" JD.int)
                (JD.field "offsetY" JD.int)
    in
    HE.on "mousemove" decoder


view : Model -> Html Msg
view { theta, lightLocation, sphere, cube, pointer, size } =
    let
        lightColor1 =
            U.updateLightColor 0.5 1 0.5

        lightColor2 =
            U.updateLightColor 1 1 0.5

        lightColor3 =
            U.updateLightColor 1 0 0

        camEye =
            V3.vec3 0 0 -1

        camCenter =
            V3.vec3 0 0 0

        camUp =
            V3.vec3 0 1 0

        perspectiveFn =
            U.globalPerspective camEye camCenter camUp size.w size.h
    in
    GL.toHtml
        [ width size.w
        , height size.h

        --, style "display" "block"
        , style "borderStyle" "solid"
        , style "borderColor" "blue"
        , onMouseMove PointerMoved
        ]
        [ -- pointer
          M3D.render
            -- shape
            sphere
            -- position
            (V3.vec3 pointer.x pointer.y 0)
            -- scale
            (V3.vec3 0.1 0.1 0.1)
            -- rotation
            (V3.vec3 0 0 0)
            lightLocation
            lightColor3
            perspectiveFn

        -- sphere 1
        --, M3D.render
        --    -- shape
        --    sphere
        --    -- position
        --    (V3.vec3 -0.5 0 0)
        --    -- scale
        --    (V3.vec3 0.3 0.3 0.3)
        --    -- rotation
        --    (V3.vec3 0 0 0)
        --    lightLocation
        --    lightColor1
        --    perspectiveFn
        -- cube 1
        --, M3D.render
        --    -- shape
        --    cube
        --    -- position
        --    (V3.vec3 0.5 0 0)
        --    -- scale
        --    (V3.vec3 0.3 0.3 0.3)
        --    -- rotation
        --    (V3.vec3 0 1 1)
        --    lightLocation
        --    lightColor2
        --    perspectiveFn
        ]
