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
    | Model3DLoaded Model3D (Result Http.Error String)


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
        [ onAnimationFrameDelta (\_ -> Ticked) ]


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

                camEyeY =
                    (V3.getY cameraEye * cosTetha) - (V3.getZ cameraEye * sinTetha)

                camEyeZ =
                    (V3.getY cameraEye * sinTetha) + (V3.getZ cameraEye * cosTetha)

                newCameraEye =
                    V3.vec3 camEyeX camEyeY camEyeZ

                newCameraUp =
                    if newTheta >= 0.0 && newTheta < 90.0 then
                        V3.vec3 0 1 0

                    else if newTheta >= 90.0 && newTheta < 180.0 then
                        V3.vec3 0 0 1

                    else if newTheta >= 180.0 && newTheta < 270.0 then
                        V3.vec3 0 -1 0

                    else
                        V3.vec3 0 0 -1

                newCamera =
                    { camera
                        | state = cameraState
                        , eye = newCameraEye
                        , up = newCameraUp
                    }
            in
            ( { model
                | theta = newTheta
                , camera = newCamera
              }
            , Cmd.none
            )

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


view : Model -> Html Msg
view model =
    UI.layout
        []
        (UI.column
            []
            [ UI.html (glView model) ]
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

        perspectiveFn =
            U.globalPerspective windowSize.w windowSize.h camera.eye camera.center camera.up
    in
    GL.toHtml
        [ width windowSize.w
        , height windowSize.h
        ]
        [ -- cube/room
          M3D.render
            objs3d.room
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
        ]
