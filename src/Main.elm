module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Debug exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Http
import Math.Matrix4 as M4
import Math.Vector3 as V3
import Object3D as Obj3D
import Objects.Cube as Cube3D
import Objects.Sphere as Sphere3D
import Utils as U
import WebGL as GL


type alias Model =
    { theta : Float
    , lightLocation : V3.Vec3
    , sphere : Obj3D.Mesh
    , cube : Obj3D.Mesh
    }


type Msg
    = Ticked
    | Sphere3DLoaded (Result Http.Error String)
    | Cube3DLoaded (Result Http.Error String)


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
    , Cmd.batch
        [ U.load3DObject "3d-models/sphere.txt" Sphere3DLoaded
        , U.load3DObject "3d-models/cube.txt" Cube3DLoaded
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
                        _ =
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
                        _ =
                            log "" httpError
                    in
                    ( model, Cmd.none )


view : Model -> Html Msg
view { lightLocation, cube, sphere } =
    let
        camera =
            U.makeCamera

        lightColor =
            U.updateLightColor

        wWidth =
            400

        wHeight =
            400

        perspectiveFn =
            globalPerspective wWidth wHeight
    in
    GL.toHtml
        [ width wWidth
        , height wHeight
        , style "display" "block"
        ]
        [ Sphere3D.render sphere (V3.vec3 0 0 0) (V3.vec3 1 1 1) (V3.vec3 1 1 1) camera lightLocation lightColor perspectiveFn
        , Cube3D.render cube (V3.vec3 0 0 0) (V3.vec3 1 1 1) (V3.vec3 1 1 1) camera lightLocation lightColor
        ]


globalPerspective : Int -> Int -> (V3.Vec3 -> M4.Mat4)
globalPerspective width height =
    let
        fn =
            \position ->
                M4.mul
                    (M4.makePerspective 120 (toFloat width / toFloat height) 0.01 100)
                    (M4.makeLookAt position (V3.add position V3.k) V3.j)
    in
    fn
