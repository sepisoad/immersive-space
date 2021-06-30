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
    = CameraPositionTop
    | CameraPositionFront
    | CameraPositionSide


type CameraState
    = RotatingTowards CameraPosition
    | RestingAt CameraPosition


type Msg
    = Ticked
    | Model3DLoaded Model3D (Result Http.Error String)
    | ChangeCameraPosition CameraPosition


type alias Camera =
    { state : CameraState
    , eye : V3.Vec3
    , eyeNew : V3.Vec3
    , center : V3.Vec3
    , up : V3.Vec3
    }


type alias Model =
    { windowSize : { w : Int, h : Int }
    , pointer : { x : Float, y : Float }
    , lightLocation : V3.Vec3
    , camera : Camera
    , zoom : Float
    , theta : Float
    , objs3d :
        { sphere : Obj3D.Mesh
        , room : Obj3D.Mesh
        , axis : Obj3D.Mesh
        }
    }


initCamera : Camera
initCamera =
    { state = RestingAt CameraPositionFront
    , eye = V3.vec3 0 0 -5
    , eyeNew = V3.vec3 0 0 -5
    , center = V3.vec3 0 0 0
    , up = V3.vec3 0 1 0
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
        initCamera
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


rotateCameraToFront : Float -> Float -> Float -> V3.Vec3 -> V3.Vec3 -> V3.Vec3 -> Camera
rotateCameraToFront theta sinTetha cosTetha eye center up =
    let
        x =
            (V3.getX eye * cosTetha) + (V3.getZ eye * sinTetha)

        y =
            V3.getY eye

        z =
            (V3.getX eye * sinTetha) - (V3.getZ eye * cosTetha)

        newEye =
            V3.vec3 x y z

        ( newUp, newState ) =
            if theta >= 0.0 && theta < 90.0 then
                ( V3.vec3 0 1 0, RotatingTowards CameraPositionFront )

            else
                ( V3.vec3 0 1 0, RestingAt CameraPositionFront )
    in
    { state = newState
    , eye = eye
    , eyeNew = newEye
    , center = center
    , up = newUp
    }


rotateCameraToTop : Float -> Float -> Float -> V3.Vec3 -> V3.Vec3 -> V3.Vec3 -> Camera
rotateCameraToTop theta sinTetha cosTetha eye center up =
    let
        x =
            V3.getX eye

        y =
            (V3.getY eye * cosTetha) - (V3.getZ eye * sinTetha)

        z =
            (V3.getY eye * sinTetha) + (V3.getZ eye * cosTetha)

        newEye =
            V3.vec3 x y z

        ( newUp, newState ) =
            if theta >= 0.0 && theta < 90.0 then
                ( V3.vec3 0 1 0, RotatingTowards CameraPositionTop )

            else
                ( V3.vec3 0 0 1, RestingAt CameraPositionTop )
    in
    { state = newState
    , eye = eye
    , eyeNew = newEye
    , center = center
    , up = newUp
    }


rotateCameraToSide : Float -> Float -> Float -> V3.Vec3 -> V3.Vec3 -> V3.Vec3 -> Camera
rotateCameraToSide theta sinTetha cosTetha eye center up =
    let
        x =
            (V3.getX eye * cosTetha) - (V3.getZ eye * sinTetha)

        y =
            V3.getY eye

        z =
            (V3.getX eye * sinTetha) + (V3.getZ eye * cosTetha)

        newEye =
            V3.vec3 x y z

        ( newUp, newState ) =
            if theta >= 0.0 && theta < 90.0 then
                ( V3.vec3 0 1 0, RotatingTowards CameraPositionSide )

            else
                ( V3.vec3 0 1 0, RestingAt CameraPositionSide )
    in
    { state = newState
    , eye = eye
    , eyeNew = newEye
    , center = center
    , up = newUp
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
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

        ChangeCameraPosition newPosition ->
            let
                camera =
                    model.camera

                newCamera =
                    { camera | state = RotatingTowards newPosition }

                log1_ =
                    log "" newCamera
            in
            ( { model | camera = newCamera, theta = 0 }, Cmd.none )

        Ticked ->
            let
                newTheta =
                    if (model.theta + 1) >= 360 then
                        0

                    else
                        model.theta + 1

                sinTetha =
                    sin (degrees newTheta)

                cosTetha =
                    cos (degrees newTheta)

                newCamera =
                    case model.camera.state of
                        RotatingTowards pos ->
                            let
                                { eye, center, up } =
                                    model.camera
                            in
                            case pos of
                                CameraPositionTop ->
                                    rotateCameraToTop newTheta sinTetha cosTetha eye center up

                                CameraPositionFront ->
                                    rotateCameraToFront newTheta sinTetha cosTetha eye center up

                                CameraPositionSide ->
                                    rotateCameraToSide newTheta sinTetha cosTetha eye center up

                        RestingAt pos ->
                            model.camera
            in
            ( { model
                | theta = newTheta
                , camera = newCamera
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    UI.layout
        []
        (UI.column
            []
            [ UI.row
                [ UI.spacing 5 ]
                [ button "front" (ChangeCameraPosition CameraPositionFront)
                , button "top" (ChangeCameraPosition CameraPositionTop)
                , button "side" (ChangeCameraPosition CameraPositionSide)
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

        perspectiveFn =
            U.globalPerspective windowSize.w windowSize.h camera.eyeNew camera.center camera.up
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
