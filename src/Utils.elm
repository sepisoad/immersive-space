module Utils exposing (..)

import Http
import Math.Matrix4 as M4
import Math.Vector3 as V3
import Maybe exposing (Maybe(..))


updateLightLocation : Float -> Float -> Float -> V3.Vec3
updateLightLocation x y z =
    V3.vec3 x y z


updateLightColor : Float -> Float -> Float -> V3.Vec3
updateLightColor r g b =
    V3.vec3 r g b



--load3DObjectOLD : String -> (Result Http.Error String -> msg) -> Cmd msg
--load3DObjectOLD url m =
--    Http.request
--        { method = "GET"
--        , headers = []
--        , url = url
--        , body = Http.emptyBody
--        , expect = Http.expectString m
--        , timeout = Nothing
--        , tracker = Nothing
--        }


load3DObject : String -> tp3d -> (tp3d -> Result Http.Error String -> msg) -> Cmd msg
load3DObject url tp m =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectString (m tp)
        , timeout = Nothing
        , tracker = Nothing
        }


degToRad : Float -> Float
degToRad deg =
    deg * (pi / 180)


globalPerspective : V3.Vec3 -> V3.Vec3 -> V3.Vec3 -> Int -> Int -> (V3.Vec3 -> V3.Vec3 -> V3.Vec3 -> M4.Mat4)
globalPerspective eye center up _ _ =
    \position scale rotation ->
        List.foldr M4.mul
            M4.identity
            [ M4.makePerspective 45 1 0.01 100
            , M4.makeLookAt eye center up
            , M4.makeTranslate position
            , M4.makeScale scale
            , M4.makeRotate (V3.getX rotation) V3.i
            , M4.makeRotate (V3.getY rotation) V3.j
            , M4.makeRotate (V3.getZ rotation) V3.k
            ]
