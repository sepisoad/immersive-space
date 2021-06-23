module Main exposing (main)

import Browser
import Browser.Events as BE exposing (onAnimationFrameDelta)
import Debug exposing (..)
import Element as UI
import Element.Background as UIB
import Element.Events as UIE
import Html exposing (Attribute, Html)
import Html.Attributes exposing (height, style, width)
import Html.Events as HE
import Http
import Json.Decode as JD
import Math.Vector3 as V3
import Model3D as M3D
import ObjectFileDecoder as Obj3D
import Utils as U
import WebGL as GL
import WebGL.Settings as GLS


type Model3D
    = Sphere
    | Room
    | Axis


type CameraPosition
    = Top
    | Front
    | Side


type CameraState
    = RotatingTowards CameraPosition
    | RestingAt CameraPosition


type Msg
    = Ticked
    | WindowResized Int Int
    | Model3DLoaded Model3D (Result Http.Error String)
    | PointerMoved Int Int
    | ZoomChanged Float
    | MoveCamera CameraPosition


type alias Model =
    { windowSize : { w : Int, h : Int }
    , pointer : { x : Float, y : Float }
    , lightLocation : V3.Vec3
    , camera :
        { state : CameraState
        , eye : V3.Vec3
        , eyeEx : V3.Vec3
        , center : V3.Vec3
        , up : V3.Vec3
        }
    , zoom : Float
    , theta : Float
    , objs3d :
        { sphere : Obj3D.Mesh
        , room : Obj3D.Mesh
        , axis : Obj3D.Mesh
        }
    }


main : Program { w : Int, h : Int } Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


init : { w : Int, h : Int } -> ( Model, Cmd Msg )
init windowSize =
    --let
    --    log1_ =
    --        log "" windowSize
    --in
    ( Model
        windowSize
        { x = 0, y = 0 }
        (U.updateLightLocation 1.3 1.3 1)
        { state = RestingAt Front
        , eye = V3.vec3 0 0 -5
        , eyeEx = V3.vec3 0 0 -5
        , center = V3.vec3 0 0 0
        , up = V3.vec3 0 1 0
        }
        2
        0.0
        { sphere = Obj3D.empty
        , room = Obj3D.empty
        , axis = Obj3D.empty
        }
    , Cmd.batch
        [ U.load3DObject "3d-models/sphere.txt" Sphere Model3DLoaded
        , U.load3DObject "3d-models/room.txt" Room Model3DLoaded
        , U.load3DObject "3d-models/axis.txt" Axis Model3DLoaded
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta (\_ -> Ticked)
        , BE.onResize WindowResized
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Ticked ->
            let
                newTheta =
                    if (model.theta + 1) >= 360 then
                        0

                    else
                        model.theta + 1

                cameraState =
                    case camera.state of
                        RotatingTowards pos ->
                            case pos of
                                Top ->
                                    RotatingTowards Top

                                Front ->
                                    RotatingTowards Front

                                Side ->
                                    RotatingTowards Side

                        RestingAt pos ->
                            RestingAt pos

                sinTetha =
                    sin (degrees newTheta)

                cosTetha =
                    cos (degrees newTheta)

                camera =
                    model.camera

                cameraEye =
                    camera.eyeEx

                camEyeX =
                    V3.getX cameraEye

                --V3.getX cameraEye * cosTetha - V3.getZ cameraEye * sinTetha
                camEyeY =
                    --V3.getY cameraEye
                    V3.getY cameraEye * cosTetha + V3.getZ cameraEye * sinTetha

                camEyeZ =
                    --V3.getZ cameraEye
                    V3.getY cameraEye * sinTetha + (V3.getZ cameraEye * cosTetha)

                newCameraEye =
                    V3.vec3 camEyeX camEyeY camEyeZ

                newCamera =
                    { camera
                        | state = cameraState
                        , eye = newCameraEye
                    }

                log1_ =
                    log "" ( sinTetha, cosTetha )

                --log "" newTheta
                --log "" ( newTheta, sinTetha )
                --log "" ( newTheta, cosTetha )
            in
            ( { model
                | theta = newTheta
                , camera = newCamera
              }
            , Cmd.none
            )

        WindowResized w h ->
            ( { model | windowSize = { w = w, h = h } }, Cmd.none )

        Model3DLoaded model3d result ->
            case result of
                Ok object ->
                    let
                        obj3d =
                            Obj3D.load object

                        objs3d =
                            model.objs3d

                        newObjs3d =
                            case model3d of
                                Sphere ->
                                    { objs3d | sphere = obj3d }

                                Room ->
                                    { objs3d | room = obj3d }

                                Axis ->
                                    { objs3d | axis = obj3d }
                    in
                    ( { model | objs3d = newObjs3d }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        PointerMoved px py ->
            let
                newX =
                    ((toFloat -px * model.zoom) / toFloat model.windowSize.w) + (model.zoom / 2)

                newY =
                    ((toFloat -py * model.zoom) / toFloat model.windowSize.h) + (model.zoom / 2)

                pointer =
                    { x = newX, y = newY }
            in
            --( { model | pointer = pointer }, Cmd.none )
            ( model, Cmd.none )

        ZoomChanged delta ->
            let
                zoom =
                    if delta >= 1 then
                        model.zoom + 0.01

                    else if delta <= -1 then
                        model.zoom - 0.01

                    else
                        model.zoom

                capZoom =
                    if zoom > 20 then
                        20

                    else if zoom < 1 then
                        1

                    else
                        zoom
            in
            ( { model | zoom = capZoom }, Cmd.none )

        MoveCamera pos ->
            let
                --towards =
                --    case pos of
                --        Front ->
                --            ( model, Cmd.none )
                --        Top ->
                --            ( model, Cmd.none )
                --        Side ->
                --            ( model, Cmd.none )
                camera =
                    model.camera

                newState =
                    RotatingTowards pos

                newCamera =
                    { camera | state = newState }
            in
            ( { model | camera = newCamera }, Cmd.none )


onMouseMove : (Int -> Int -> Msg) -> Attribute Msg
onMouseMove msg =
    let
        decoder =
            JD.map2 msg
                (JD.field "offsetX" JD.int)
                (JD.field "offsetY" JD.int)
    in
    HE.on "mousemove" decoder


onMouseWheel : (Float -> Msg) -> Attribute Msg
onMouseWheel msg =
    let
        decoder =
            JD.map msg
                (JD.field "deltaY" JD.float)

        attrMsg =
            HE.on "wheel" decoder
    in
    attrMsg


view : Model -> Html Msg
view model =
    UI.layout
        []
        (UI.column
            []
            [ UI.row [ UI.spacing 5 ]
                [ button "front" (MoveCamera Front)
                , button "top" (MoveCamera Top)
                , button "side" (MoveCamera Side)
                ]
            , UI.html (glView model)
            ]
        )


button : String -> Msg -> UI.Element Msg
button label msg =
    UI.el
        [ UI.padding 5
        , UI.pointer
        , UIB.color (UI.rgb 0.5 0.5 0.5)
        , UIE.onClick msg
        ]
        (UI.text label)


glView : Model -> Html Msg
glView { theta, lightLocation, pointer, windowSize, zoom, objs3d, camera } =
    let
        lightColor1 =
            U.updateLightColor 0.5 0.5 0.5

        lightColor2 =
            U.updateLightColor 0.3 0.3 0.3

        perspectiveFn =
            U.globalPerspective camera.eye camera.center camera.up 1 1
    in
    GL.toHtml
        [ width windowSize.w
        , height windowSize.h

        --, style "border" "solid 0.2em"
        , onMouseMove PointerMoved
        , onMouseWheel ZoomChanged
        ]
        [ -- cube/room
          M3D.render
            --[ GLS.cullFace GLS.back ]
            -- shape
            objs3d.axis
            -- color
            (V3.vec3 1 1 0)
            -- position
            (V3.vec3 0 0 0)
            -- scale
            (V3.vec3 1 1 1)
            -- rotation
            (V3.vec3 0 0 0)
            lightLocation
            lightColor1
            perspectiveFn

        ---- sphere 1
        --, M3D.render
        --    [ GLS.cullFace GLS.back ]
        --    -- shape
        --    objs3d.sphere
        --    -- color
        --    (V3.vec3 0 1 0)
        --    -- position
        --    (V3.vec3 pointer.x pointer.y zoom)
        --    -- scale
        --    (V3.vec3 0.2 0.2 0.2)
        --    -- rotation
        --    (V3.vec3 0 0 0)
        --    lightLocation
        --    lightColor2
        --    perspectiveFn
        ]
