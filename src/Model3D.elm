module Model3D exposing (..)

import Debug exposing (..)
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe exposing (Maybe(..))
import ObjectFileDecoder as Obj3D
import WebGL as GL
import WebGL.Settings exposing (Setting)


type alias Vertex =
    { position : Vec3
    , normal : Vec3

    --, color : Vec3
    }


emptyVertex : ( Vertex, Vertex, Vertex )
emptyVertex =
    ( Vertex (vec3 0 0 0) (vec3 0 0 0)
    , Vertex (vec3 0 0 0) (vec3 0 0 0)
    , Vertex (vec3 0 0 0) (vec3 0 0 0)
    )


type alias Uniforms =
    { lightLocation : Vec3
    , lightColor : Vec3
    , color : Vec3
    , perspective : Mat4
    }


cretaeGLMesh : Obj3D.Mesh -> Vec3 -> GL.Mesh Vertex
cretaeGLMesh mesh _ =
    let
        vertices =
            List.foldr
                (\triangle lst ->
                    let
                        maybePosition =
                            triangle.position

                        maybeNormal =
                            triangle.normal

                        --maybeUV =
                        --    triangle.uv
                        vertex =
                            case ( maybePosition, maybeNormal ) of
                                ( Just pos, Just normal ) ->
                                    let
                                        ( pa, pb, pc ) =
                                            pos

                                        ( na, nb, nc ) =
                                            normal
                                    in
                                    ( Vertex (vec3 pa.x pa.y pa.z) (vec3 na.x na.y na.z)
                                    , Vertex (vec3 pb.x pb.y pb.z) (vec3 nb.x nb.y nb.z)
                                    , Vertex (vec3 pc.x pc.y pc.z) (vec3 nc.x nc.y nc.z)
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


vertexShader : GL.Shader Vertex Uniforms { vnormal : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        uniform mat4 perspective;
        varying vec3 vnormal;
        void main () {            
            gl_Position = perspective * vec4(position, 1.0);
            vnormal = normal;
        }
        
    |]


fragmentShader : GL.Shader {} Uniforms { vnormal : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 lightLocation;
        uniform vec3 lightColor;   
        uniform vec3 color;
        varying vec3 vnormal;
        void main () {
            vec3 normal = normalize(vnormal);
            float light = dot(normal, lightLocation);
            gl_FragColor = vec4((lightColor * color), 1);
            gl_FragColor.rgb *= light;
        }
        
    |]


render : List Setting -> Obj3D.Mesh -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> (Vec3 -> Vec3 -> Vec3 -> Mat4) -> GL.Entity
render settings shape color position scale rotation lightLocation lightColor perspectiveFn =
    let
        perspective =
            perspectiveFn position scale rotation
    in
    GL.entityWith
        settings
        vertexShader
        fragmentShader
        (cretaeGLMesh shape position)
        (Uniforms lightLocation lightColor color perspective)
