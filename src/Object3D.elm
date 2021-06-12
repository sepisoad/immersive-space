module Object3D exposing
    ( Mesh
    , Normal
    , Position
    , Triangle
    , UV
    , empty
    , emptyNormal
    , emptyPosition
    , emptyUV
    , load
    )

import Array exposing (Array)
import Debug exposing (..)
import Maybe exposing (Maybe(..))
import Parser exposing (..)
import Tuple3 as T3


type alias Normal =
    { x : Float, y : Float, z : Float }


emptyNormal : Normal
emptyNormal =
    Normal 0 0 0


type alias Position =
    { x : Float, y : Float, z : Float }


emptyPosition : Position
emptyPosition =
    Position 0 0 0


type alias UV =
    { a : Float, b : Float }


emptyUV : UV
emptyUV =
    UV 0 0


type Face
    = Face
        { a : Int
        , b : Int
        , c : Int
        }
    | FaceUV
        { a : ( Int, Int )
        , b : ( Int, Int )
        , c : ( Int, Int )
        }
    | FaceNormal
        { a : ( Int, Int )
        , b : ( Int, Int )
        , c : ( Int, Int )
        }
    | FaceUVNormal
        { a : ( Int, Int, Int )
        , b : ( Int, Int, Int )
        , c : ( Int, Int, Int )
        }


faceIndices : Int -> Int -> Int -> ( Int, Int, Int )
faceIndices a b c =
    ( a, b, c )


makeFace : Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Face
makeFace a1 a2 a3 b1 b2 b3 c1 c2 c3 =
    case [ a1, a2, a3, b1, b2, b3, c1, c2, c3 ] of
        [ Just va1, Just va2, Just va3, Just vb1, Just vb2, Just vb3, Just vc1, Just vc2, Just vc3 ] ->
            FaceUVNormal
                { a = ( va1, va2, va3 )
                , b = ( vb1, vb2, vb3 )
                , c = ( vc1, vc2, vc3 )
                }

        [ Just va1, Nothing, Just va3, Just vb1, Nothing, Just vb3, Just vc1, Nothing, Just vc3 ] ->
            FaceNormal
                { a = ( va1, va3 )
                , b = ( vb1, vb3 )
                , c = ( vc1, vc3 )
                }

        [ Just va1, Just va2, Just vb1, Just vb2, Just vc1, Just vc2 ] ->
            FaceUV
                { a = ( va1, va2 )
                , b = ( vb1, vb2 )
                , c = ( vc1, vc2 )
                }

        [ Just va1, Just va2, Just va3 ] ->
            Face { a = va1, b = va2, c = va3 }

        _ ->
            Face { a = 0, b = 0, c = 0 }


type alias Triangle =
    { position : Maybe ( Position, Position, Position )
    , uv : Maybe ( UV, UV, UV )
    , normal : Maybe ( Normal, Normal, Normal )
    }


makeTriangle : Maybe Position -> Maybe Position -> Maybe Position -> Maybe UV -> Maybe UV -> Maybe UV -> Maybe Normal -> Maybe Normal -> Maybe Normal -> Triangle
makeTriangle p1 p2 p3 u1 u2 u3 n1 n2 n3 =
    let
        mlpos =
            case ( p1, p2, p3 ) of
                ( Just vp1, Just vp2, Just vp3 ) ->
                    Just ( vp1, vp2, vp3 )

                _ ->
                    Nothing

        mluv =
            case ( u1, u2, u3 ) of
                ( Just vu1, Just vu2, Just vu3 ) ->
                    Just ( vu1, vu2, vu3 )

                _ ->
                    Nothing

        mlnor =
            case ( n1, n2, n3 ) of
                ( Just vn1, Just vn2, Just vn3 ) ->
                    Just ( vn1, vn2, vn3 )

                _ ->
                    Nothing
    in
    Triangle mlpos mluv mlnor



--type alias Quad =
--    { a : Triangle, b : Triangle }
--makeQuad : Maybe Triangle -> Maybe Triangle -> Maybe Quad
--makeQuad ma mb =
--    case ( ma, mb ) of
--        ( Just va, Just vb ) ->
--            Just (Quad va vb)
--        _ ->
--            Nothing


type alias Mesh =
    List Triangle



--makeMesh : List (Maybe Quad) -> List Quad
--makeMesh lmq =
--    List.foldr
--        (\item arr ->
--            case item of
--                Just v ->
--                    List.append [ v ] arr
--                _ ->
--                    arr
--        )
--        []
--        lmq


empty : Mesh
empty =
    []


type alias MeshRaw =
    { positions : Array Position
    , normals : Array Normal
    , uvs : Array UV
    , faces : Array Face
    }


updatePositions : MeshRaw -> Position -> MeshRaw
updatePositions mesh position =
    let
        newPosition =
            Array.push position mesh.positions
    in
    { mesh | positions = newPosition }


updateNormals : MeshRaw -> Normal -> MeshRaw
updateNormals mesh normal =
    let
        newNormal =
            Array.push normal mesh.normals
    in
    { mesh | normals = newNormal }


updateUVs : MeshRaw -> UV -> MeshRaw
updateUVs mesh uv =
    let
        newUV =
            Array.push uv mesh.uvs
    in
    { mesh | uvs = newUV }


updateFaces : MeshRaw -> Face -> MeshRaw
updateFaces mesh face =
    let
        newFaces =
            Array.push face mesh.faces
    in
    { mesh | faces = newFaces }


makeEmptyMesh : MeshRaw
makeEmptyMesh =
    MeshRaw Array.empty Array.empty Array.empty Array.empty


floatNumber : Parser Float
floatNumber =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= float
        , float
        ]


parsePosition : Parser Position
parsePosition =
    succeed Position
        |. keyword "v"
        |. spaces
        |= floatNumber
        |. spaces
        |= floatNumber
        |. spaces
        |= floatNumber


parseUV : Parser UV
parseUV =
    succeed UV
        |. keyword "vt"
        |. spaces
        |= float
        |. spaces
        |= float


parseNormal : Parser Normal
parseNormal =
    succeed Normal
        |. keyword "vn"
        |. spaces
        |= floatNumber
        |. spaces
        |= floatNumber
        |. spaces
        |= floatNumber


nullOrInt : Parser (Maybe Int)
nullOrInt =
    oneOf
        [ map Just int
        , map (\_ -> Nothing) (succeed ())
        ]


parseFace : Parser Face
parseFace =
    succeed makeFace
        |. keyword "f"
        |. spaces
        |= nullOrInt
        |. symbol "/"
        |= nullOrInt
        |. symbol "/"
        |= nullOrInt
        |. spaces
        |= nullOrInt
        |. symbol "/"
        |= nullOrInt
        |. symbol "/"
        |= nullOrInt
        |. spaces
        |= nullOrInt
        |. symbol "/"
        |= nullOrInt
        |. symbol "/"
        |= nullOrInt


whateverEles : Parser ()
whateverEles =
    succeed ()
        |. chompIf (\_ -> True)


type ParsedValue
    = FaceValue Face
    | PositionValue Position
    | NormalValue Normal
    | UVValue UV
    | NoValue


parse : String -> Result (List DeadEnd) Face
parse line =
    run parseFace line


load : String -> Mesh
load data =
    let
        lines =
            String.split "\n" data

        resultList =
            List.map
                (\line ->
                    run
                        (oneOf
                            [ map (\v -> FaceValue v) parseFace
                            , map (\v -> PositionValue v) parsePosition
                            , map (\v -> NormalValue v) parseNormal
                            , map (\v -> UVValue v) parseUV
                            , map (\v -> NoValue) whateverEles

                            --, map (\v -> NoValue) (chompUntilEndOr "\n")
                            --, map (\v -> NoValue) (chompUntilEndOr "g")
                            ]
                        )
                        line
                )
                lines

        values =
            List.map
                (\result ->
                    case result of
                        Ok value ->
                            value

                        Err msg ->
                            NoValue
                )
                resultList

        rawMeshes =
            List.foldl
                (\value mesh ->
                    case value of
                        FaceValue face ->
                            updateFaces mesh face

                        PositionValue position ->
                            updatePositions mesh position

                        NormalValue normal ->
                            updateNormals mesh normal

                        UVValue uv ->
                            updateUVs mesh uv

                        NoValue ->
                            mesh
                )
                makeEmptyMesh
                values

        dec =
            \a -> a - 1

        getPosition =
            \i ->
                Maybe.withDefault emptyPosition (Array.get (dec i) rawMeshes.positions)

        getNormal =
            \i ->
                Maybe.withDefault emptyNormal (Array.get (dec i) rawMeshes.normals)

        getUV =
            \i ->
                Maybe.withDefault emptyUV (Array.get (dec i) rawMeshes.uvs)

        --fixTupIndex =
        --    \tup3 ->
        --        T3.mapAllThree
        --            dec
        --            dec
        --            dec
        --            tup3
        triangleArr =
            Array.map
                (\face ->
                    let
                        triangle =
                            case face of
                                Face f ->
                                    Triangle
                                        (Just
                                            ( getPosition f.a
                                            , getPosition f.b
                                            , getPosition f.c
                                            )
                                        )
                                        Nothing
                                        Nothing

                                FaceUV fu ->
                                    let
                                        ( p1, u1 ) =
                                            fu.a

                                        ( p2, u2 ) =
                                            fu.b

                                        ( p3, u3 ) =
                                            fu.c
                                    in
                                    Triangle
                                        (Just
                                            ( getPosition p1
                                            , getPosition p2
                                            , getPosition p3
                                            )
                                        )
                                        (Just
                                            ( getUV p1
                                            , getUV p2
                                            , getUV p3
                                            )
                                        )
                                        Nothing

                                FaceNormal fn ->
                                    let
                                        ( p1, n1 ) =
                                            fn.a

                                        ( p2, n2 ) =
                                            fn.b

                                        ( p3, n3 ) =
                                            fn.c
                                    in
                                    Triangle
                                        (Just
                                            ( getPosition p1
                                            , getPosition p2
                                            , getPosition p3
                                            )
                                        )
                                        Nothing
                                        (Just
                                            ( getNormal n1
                                            , getNormal n2
                                            , getNormal n3
                                            )
                                        )

                                FaceUVNormal fun ->
                                    let
                                        ( p1, u1, n1 ) =
                                            fun.a

                                        ( p2, u2, n2 ) =
                                            fun.b

                                        ( p3, u3, n3 ) =
                                            fun.c
                                    in
                                    Triangle
                                        (Just
                                            ( getPosition p1
                                            , getPosition p2
                                            , getPosition p3
                                            )
                                        )
                                        (Just
                                            ( getUV p1
                                            , getUV p2
                                            , getUV p3
                                            )
                                        )
                                        (Just
                                            ( getNormal n1
                                            , getNormal n2
                                            , getNormal n3
                                            )
                                        )
                    in
                    triangle
                )
                rawMeshes.faces
    in
    Array.toList triangleArr
