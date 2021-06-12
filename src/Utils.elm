module Utils exposing (..)

import Http
import Math.Matrix4 as M4
import Math.Vector3 as V3
import Maybe exposing (Maybe(..))
import Object3D as Obj3D


makeCamera : M4.Mat4
makeCamera =
    M4.mul
        (M4.makePerspective 45 1 0.01 100)
        (M4.makeLookAt (V3.vec3 0 0 10) (V3.vec3 0 0 0) (V3.vec3 0 1 0))


updateLightLocation : Float -> Float -> Float -> V3.Vec3
updateLightLocation x y z =
    V3.vec3 x y z


updateLightColor : V3.Vec3
updateLightColor =
    V3.vec3 0.5 1 0.5


load3DObject : String -> (Result Http.Error String -> msg) -> Cmd msg
load3DObject url m =
    Http.get
        { url = url
        , expect = Http.expectString m
        }


degToRad : Float -> Float
degToRad deg =
    deg * (pi / 180)
