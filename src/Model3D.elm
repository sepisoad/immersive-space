module Model3D exposing (..)

import Debug exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Maybe exposing (Maybe(..), withDefault)
import ObjectFileDecoder as Obj3D
import Utils as U
import WebGL as GL


type alias Vertex =
    { position : Vec3
    , normal : Vec3
    , color : Vec3
    }


emptyVertex : ( Vertex, Vertex, Vertex )
emptyVertex =
    ( Vertex (vec3 0 0 0) (vec3 0 0 0) (vec3 0 0 0)
    , Vertex (vec3 0 0 0) (vec3 0 0 0) (vec3 0 0 0)
    , Vertex (vec3 0 0 0) (vec3 0 0 0) (vec3 0 0 0)
    )


type alias Uniforms =
    { lightLocation : Vec3
    , lightColor : Vec3
    , perspective : Mat4
    }


cretaeGLMesh : Obj3D.Mesh -> Vec3 -> GL.Mesh Vertex
cretaeGLMesh mesh position =
    let
        vertices =
            List.foldr
                (\triangle lst ->
                    let
                        maybePosition =
                            triangle.position

                        maybeNormal =
                            triangle.normal

                        maybeUV =
                            triangle.uv

                        vertex =
                            case ( maybePosition, maybeNormal ) of
                                ( Just pos, Just normal ) ->
                                    let
                                        ( pa, pb, pc ) =
                                            pos

                                        ( na, nb, nc ) =
                                            normal
                                    in
                                    ( Vertex (vec3 pa.x pa.y pa.z) (vec3 na.x na.y na.z) (vec3 pa.x pa.y pa.z)
                                    , Vertex (vec3 pb.x pb.y pb.z) (vec3 nb.x nb.y nb.z) (vec3 pb.x pb.y pb.z)
                                    , Vertex (vec3 pc.x pc.y pc.z) (vec3 nc.x nc.y nc.z) (vec3 pc.x pc.y pc.z)
                                    )

                                _ ->
                                    emptyVertex
                    in
                    List.append lst [ vertex ]
                )
                []
                mesh
    in
    GL.triangles
        vertices


vertexShader : GL.Shader Vertex Uniforms { vcolor : Vec3, vnormal : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;
        varying vec3 vnormal;
        void main () {            
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
            vnormal = normal;
        }
        
    |]


fragmentShader : GL.Shader {} Uniforms { vcolor : Vec3, vnormal : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 lightLocation;
        uniform vec3 lightColor;        
        varying vec3 vcolor;
        varying vec3 vnormal;
        void main () {
            vec3 normal = normalize(vnormal);
            float light = dot(normal, lightLocation);
            gl_FragColor = vec4(lightColor, 1);
            gl_FragColor.rgb *= light;
        }
        
    |]


render : Obj3D.Mesh -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> (Vec3 -> Vec3 -> Vec3 -> Mat4) -> GL.Entity
render shape position scale rotation lightLocation lightColor perspectiveFn =
    let
        perspective =
            perspectiveFn position scale rotation
    in
    GL.entity
        vertexShader
        fragmentShader
        (cretaeGLMesh shape position)
        (Uniforms lightLocation lightColor perspective)
