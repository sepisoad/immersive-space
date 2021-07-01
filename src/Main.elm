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
    , stateOld : CameraState
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
    , stateOld = RestingAt CameraPositionFront
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


rotateCameraToTopFromFront : Float -> V3.Vec3 -> V3.Vec3 -> V3.Vec3 -> ( Camera, Float )
rotateCameraToTopFromFront theta eye center up =
    let
        newTheta =
            theta + 1

        sinTheta =
            sin (degrees newTheta)

        cosTheta =
            cos (degrees newTheta)

        x =
            V3.getX eye

        y =
            (V3.getY eye * cosTheta) - (V3.getZ eye * sinTheta)

        z =
            (V3.getY eye * sinTheta) + (V3.getZ eye * cosTheta)

        eyeNew =
            V3.vec3 x y z

        ( eyeEx, newUp, ( stateNew, stateOld ) ) =
            if newTheta <= 90.0 then
                ( eye, V3.vec3 0 1 0, ( RotatingTowards CameraPositionTop, RestingAt CameraPositionFront ) )

            else
                ( eyeNew, V3.vec3 0 0 1, ( RestingAt CameraPositionTop, RestingAt CameraPositionTop ) )
    in
    ( { state = stateNew
      , stateOld = stateOld
      , eye = eye
      , eyeNew = eyeNew
      , center = center
      , up = newUp
      }
    , newTheta
    )


rotateCameraToTopFromSide : Float -> V3.Vec3 -> V3.Vec3 -> V3.Vec3 -> ( Camera, Float )
rotateCameraToTopFromSide theta eye center up =
    let
        newTheta =
            theta + 1

        sinTheta =
            sin (degrees newTheta)

        cosTheta =
            cos (degrees newTheta)

        x =
            V3.getX eye

        y =
            (V3.getY eye * cosTheta) - (V3.getZ eye * sinTheta)

        z =
            (V3.getY eye * sinTheta) + (V3.getZ eye * cosTheta)

        eyeNew =
            V3.vec3 x y z

        log1_ =
            log "" theta

        ( eyeEx, newUp, ( stateNew, stateOld ) ) =
            if newTheta <= 90.0 then
                ( eye, V3.vec3 0 1 0, ( RotatingTowards CameraPositionTop, RestingAt CameraPositionSide ) )

            else
                ( eyeNew, V3.vec3 0 0 1, ( RestingAt CameraPositionTop, RestingAt CameraPositionTop ) )
    in
    ( { state = stateNew
      , stateOld = stateOld
      , eye = eye
      , eyeNew = eyeNew
      , center = center
      , up = newUp
      }
    , newTheta
    )


rotateCameraToFrontFromSide : Float -> V3.Vec3 -> V3.Vec3 -> V3.Vec3 -> ( Camera, Float )
rotateCameraToFrontFromSide theta eye center up =
    let
        newTheta =
            theta - 1

        sinTheta =
            sin (degrees newTheta)

        cosTheta =
            cos (degrees newTheta)

        x =
            (V3.getX eye * cosTheta) - (V3.getZ eye * sinTheta)

        y =
            V3.getY eye

        z =
            (V3.getX eye * sinTheta) + (V3.getZ eye * cosTheta)

        eyeNew =
            V3.vec3 x y z

        ( stateNew, stateOld, thetaNew ) =
            if newTheta > 0.0 then
                ( RotatingTowards CameraPositionFront, RestingAt CameraPositionSide, newTheta )

            else
                ( RestingAt CameraPositionFront, RestingAt CameraPositionFront, 0 )
    in
    ( { state = stateNew
      , stateOld = stateOld
      , eye = eye
      , eyeNew = eyeNew
      , center = center
      , up = up
      }
    , thetaNew
    )


rotateCameraToFrontFromTop : Float -> V3.Vec3 -> V3.Vec3 -> V3.Vec3 -> ( Camera, Float )
rotateCameraToFrontFromTop theta eye center up =
    let
        newTheta =
            theta - 1

        sinTheta =
            sin (degrees newTheta)

        cosTheta =
            cos (degrees newTheta)

        x =
            V3.getX eye

        y =
            (V3.getY eye * cosTheta) - (V3.getZ eye * sinTheta)

        z =
            (V3.getY eye * sinTheta) + (V3.getZ eye * cosTheta)

        eyeNew =
            V3.vec3 x y z

        { upNew, stateNew, stateOld, thetaNew } =
            if newTheta > 0.0 then
                { upNew = V3.vec3 0 0 1
                , stateNew = RotatingTowards CameraPositionFront
                , stateOld = RestingAt CameraPositionTop
                , thetaNew = newTheta
                }

            else
                { upNew = V3.vec3 0 1 0
                , stateNew = RestingAt CameraPositionFront
                , stateOld = RestingAt CameraPositionFront
                , thetaNew = newTheta
                }
    in
    ( { state = stateNew
      , stateOld = stateOld
      , eye = eye
      , eyeNew = eyeNew
      , center = center
      , up = upNew
      }
    , thetaNew
    )


rotateCameraToSideFromFront : Float -> V3.Vec3 -> V3.Vec3 -> V3.Vec3 -> ( Camera, Float )
rotateCameraToSideFromFront theta eye center up =
    let
        newTheta =
            theta + 1

        sinTheta =
            sin (degrees newTheta)

        cosTheta =
            cos (degrees newTheta)

        x =
            (V3.getX eye * cosTheta) - (V3.getZ eye * sinTheta)

        y =
            V3.getY eye

        z =
            (V3.getX eye * sinTheta) + (V3.getZ eye * cosTheta)

        eyeNew =
            V3.vec3 x y z

        ( stateNew, stateOld, thetaNew ) =
            if newTheta <= 90.0 then
                ( RotatingTowards CameraPositionSide, RestingAt CameraPositionFront, newTheta )

            else
                ( RestingAt CameraPositionSide, RestingAt CameraPositionSide, 0 )
    in
    ( { state = stateNew
      , stateOld = stateOld
      , eye = eye
      , eyeNew = eyeNew
      , center = center
      , up = up
      }
    , thetaNew
    )


rotateCameraToSideFromTop : Float -> V3.Vec3 -> V3.Vec3 -> V3.Vec3 -> ( Camera, Float )
rotateCameraToSideFromTop theta eye center up =
    let
        newTheta =
            theta - 1

        sinTheta =
            sin (degrees newTheta)

        cosTheta =
            cos (degrees newTheta)

        x =
            (V3.getX eye * cosTheta) - (V3.getZ eye * sinTheta)

        y =
            V3.getY eye

        z =
            (V3.getX eye * sinTheta) + (V3.getZ eye * cosTheta)

        eyeNew =
            V3.vec3 x y z

        { upNew, stateNew, stateOld, thetaNew } =
            if newTheta > 0.0 then
                { upNew = V3.vec3 0 0 1
                , stateNew = RotatingTowards CameraPositionSide
                , stateOld = RestingAt CameraPositionTop
                , thetaNew = newTheta
                }

            else
                { upNew = V3.vec3 0 1 0
                , stateNew = RestingAt CameraPositionSide
                , stateOld = RestingAt CameraPositionSide
                , thetaNew = 0.0
                }
    in
    ( { state = stateNew
      , stateOld = stateOld
      , eye = eye
      , eyeNew = eyeNew
      , center = center
      , up = upNew
      }
    , thetaNew
    )


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

        ChangeCameraPosition newPos ->
            let
                camera =
                    model.camera

                newCamera =
                    { camera | state = RotatingTowards newPos }
            in
            case camera.state of
                RotatingTowards _ ->
                    ( model, Cmd.none )

                RestingAt pos ->
                    if pos /= newPos then
                        ( { model | camera = newCamera }, Cmd.none )

                    else
                        ( model, Cmd.none )

        Ticked ->
            let
                ( newCamera, thetaNew ) =
                    case model.camera.state of
                        RotatingTowards pos ->
                            let
                                { eye, center, up, state, stateOld } =
                                    model.camera

                                oldPos =
                                    case stateOld of
                                        RotatingTowards p ->
                                            p

                                        RestingAt p ->
                                            p

                                theta =
                                    model.theta
                            in
                            case pos of
                                CameraPositionTop ->
                                    case oldPos of
                                        CameraPositionFront ->
                                            rotateCameraToTopFromFront theta eye center up

                                        CameraPositionSide ->
                                            rotateCameraToTopFromSide theta eye center up

                                        _ ->
                                            ( model.camera, model.theta )

                                CameraPositionFront ->
                                    case oldPos of
                                        CameraPositionSide ->
                                            rotateCameraToFrontFromSide theta eye center up

                                        CameraPositionTop ->
                                            rotateCameraToFrontFromTop theta eye center up

                                        _ ->
                                            ( model.camera, model.theta )

                                CameraPositionSide ->
                                    case oldPos of
                                        CameraPositionFront ->
                                            rotateCameraToSideFromFront theta eye center up

                                        CameraPositionTop ->
                                            rotateCameraToSideFromTop theta eye center up

                                        _ ->
                                            ( model.camera, model.theta )

                        RestingAt pos ->
                            ( model.camera, model.theta )
            in
            ( { model
                | theta = thetaNew
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
