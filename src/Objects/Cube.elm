module Objects.Cube exposing (render)

import Debug exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Maybe exposing (Maybe(..), withDefault)
import Object3D as Obj3D
import Objects.Utils as U
import WebGL as GL


type alias Vertex =
    { position : Vec3
    , uv : Vec2
    , normal : Vec3
    , color : Vec3
    }


emptyVertex : ( Vertex, Vertex, Vertex )
emptyVertex =
    ( Vertex (vec3 0 0 0) (vec2 0 0) (vec3 0 0 0) (vec3 0 0 0)
    , Vertex (vec3 0 0 0) (vec2 0 0) (vec3 0 0 0) (vec3 0 0 0)
    , Vertex (vec3 0 0 0) (vec2 0 0) (vec3 0 0 0) (vec3 0 0 0)
    )



--( Vertex (vec3 0 0 0) (vec3 0 0 0) (vec3 0 0 0)
--, Vertex (vec3 0 0 0) (vec3 0 0 0) (vec3 0 0 0)
--, Vertex (vec3 0 0 0) (vec3 0 0 0) (vec3 0 0 0)
--)


type alias Uniforms =
    { camera : Mat4
    , lightLocation : Vec3
    , lightColor : Vec3
    }


cretaeGLMesh : Obj3D.Mesh -> GL.Mesh Vertex
cretaeGLMesh mesh =
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
                            case ( maybePosition, maybeNormal, maybeUV ) of
                                ( Just pos, Just normal, Just uv ) ->
                                    let
                                        ( pa, pb, pc ) =
                                            pos

                                        ( na, nb, nc ) =
                                            normal

                                        ( ua, ub, uc ) =
                                            uv
                                    in
                                    ( Vertex (vec3 pa.x pa.y pa.z) (vec2 ua.a ua.b) (vec3 na.x na.y na.z) (vec3 na.x na.y na.z)
                                    , Vertex (vec3 pb.x pb.y pb.z) (vec2 ub.a ub.b) (vec3 nb.x nb.y nb.z) (vec3 nb.x nb.y nb.z)
                                    , Vertex (vec3 pc.x pc.y pc.z) (vec2 uc.a uc.b) (vec3 nc.x nc.y nc.z) (vec3 nc.x nc.y nc.z)
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
        //attribute vec2 uv;
        attribute vec3 normal;
        attribute vec3 color;
        uniform mat4 camera;
        varying vec3 vcolor;
        varying vec3 vnormal;
        void main () {
            gl_Position = camera * vec4(position, 1.0);
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
        //uniform vec4 u_color;
        varying vec3 vcolor;
        varying vec3 vnormal;
        void main () {
            vec3 normal = normalize(vnormal);
            float light = dot(normal, lightLocation);
            gl_FragColor = vec4(vcolor, 1);
            gl_FragColor.rgb *= light;
        }
        
    |]


render : Obj3D.Mesh -> Vec3 -> Vec3 -> Vec3 -> Mat4 -> Vec3 -> Vec3 -> GL.Entity
render cube position scale rotation camera lightLocation lightColor =
    GL.entity
        vertexShader
        fragmentShader
        (cretaeGLMesh cube)
        (Uniforms camera lightLocation lightColor)
