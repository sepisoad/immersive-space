module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Debug exposing (..)
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


type Model3D
    = Sphere
    | Cube
    | Room
    | Axis


type Msg
    = Ticked
    | Model3DLoaded Model3D (Result Http.Error String)
    | PointerMoved Int Int
    | ZoomChanged Float


type alias Model =
    { theta : Float
    , lightLocation : V3.Vec3
    , sphere : Obj3D.Mesh
    , cube : Obj3D.Mesh
    , pointer : { x : Float, y : Float }
    , size : { w : Int, h : Int }
    , zoom : Float
    , objs3d :
        { sphere : Obj3D.Mesh
        , cube : Obj3D.Mesh
        , room : Obj3D.Mesh
        , axis : Obj3D.Mesh
        }
    }


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
        2
        { sphere = Obj3D.empty
        , cube = Obj3D.empty
        , room = Obj3D.empty
        , axis = Obj3D.empty
        }
    , Cmd.batch
        [ U.load3DObject "3d-models/sphere.txt" Sphere Model3DLoaded
        , U.load3DObject "3d-models/cube.txt" Cube Model3DLoaded
        , U.load3DObject "3d-models/room.txt" Room Model3DLoaded
        , U.load3DObject "3d-models/axis.txt" Axis Model3DLoaded
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
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

        Model3DLoaded model3d result ->
            let
                log1_ =
                    log "" model3d
            in
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

                                Cube ->
                                    { objs3d | cube = obj3d }

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
                    ((toFloat -px * model.zoom) / toFloat model.size.w) + (model.zoom / 2)

                newY =
                    ((toFloat -py * model.zoom) / toFloat model.size.h) + (model.zoom / 2)

                pointer =
                    { x = newX, y = newY }
            in
            ( { model | pointer = pointer }, Cmd.none )

        ZoomChanged delta ->
            let
                _ =
                    log "" delta

                zoom =
                    if delta >= 1 then
                        model.zoom + 1

                    else if delta <= -1 then
                        model.zoom - 1

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
view { theta, lightLocation, pointer, size, zoom, objs3d } =
    let
        lightColor1 =
            U.updateLightColor 0.5 1 0.5

        camEye =
            V3.vec3 0 0 -0.5

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
        , style "display" "block"
        , style "borderStyle" "solid"
        , style "borderColor" "blue"
        , onMouseMove PointerMoved
        , onMouseWheel ZoomChanged
        ]
        [ -- pointer
          M3D.render
            -- shape
            objs3d.axis
            -- position
            (V3.vec3 pointer.x pointer.y zoom)
            -- scale
            (V3.vec3 1 1 1)
            -- rotation
            (V3.vec3 0 0 0)
            lightLocation
            lightColor1
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
