module Objects.Utils exposing (..)

import Debug exposing (..)
import Math.Vector3 as V3
import Object3D as Obj3D
import Tuple3 as T3


positionToVec3Tup : ( Obj3D.Position, Obj3D.Position, Obj3D.Position ) -> ( V3.Vec3, V3.Vec3, V3.Vec3 )
positionToVec3Tup ( a, b, c ) =
    let
        tup =
            ( V3.vec3 a.x a.y a.z
            , V3.vec3 b.x b.y b.z
            , V3.vec3 c.x c.y c.z
            )

        --log1_ =
        --    log "" tup
    in
    tup
