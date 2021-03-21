module Mesh exposing
    ( Mesh
    , combine
    , deduplicateVertices
    , edgeIndices
    , edgeVertices
    , empty
    , faceIndices
    , faceVertices
    , faces
    , fromTriangularMesh
    , indexed
    , mapVertices
    , neighbors
    , subdivide
    , toTriangularMesh
    , vertex
    , vertices
    , withNormals
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set
import TriangularMesh exposing (TriangularMesh)


type Mesh vertex
    = Mesh { vertices : Array vertex, faceIndices : List (List Int) }


empty : Mesh vertex
empty =
    Mesh { vertices = Array.empty, faceIndices = [] }


indexed : Array vertex -> List (List Int) -> Mesh vertex
indexed vertices_ faceIndices_ =
    let
        numVertices =
            Array.length vertices_

        validIndex i =
            i >= 0 && i < numVertices

        goodIndices =
            List.map (List.filter validIndex) faceIndices_
                |> List.filter (\is -> List.length is > 0)
    in
    Mesh { vertices = vertices_, faceIndices = goodIndices }


faces : List (List vertex) -> Mesh vertex
faces faceVertices_ =
    let
        vertices_ =
            List.concat faceVertices_
                |> Array.fromList

        makeIndices offset lengths =
            case lengths of
                n :: rest ->
                    List.range offset (offset + n - 1)
                        :: makeIndices (offset + n) rest

                _ ->
                    []

        faceIndices_ =
            List.map List.length faceVertices_ |> makeIndices 0
    in
    Mesh { vertices = vertices_, faceIndices = faceIndices_ }


vertices : Mesh vertex -> Array vertex
vertices (Mesh mesh) =
    mesh.vertices


faceIndices : Mesh vertex -> List (List Int)
faceIndices (Mesh mesh) =
    mesh.faceIndices


faceVertices : Mesh vertex -> List (List vertex)
faceVertices mesh =
    let
        toFace =
            List.filterMap (\i -> vertex i mesh)
    in
    List.map toFace (faceIndices mesh)


vertex : Int -> Mesh vertex -> Maybe vertex
vertex index mesh =
    Array.get index (vertices mesh)


asEdge : Int -> Int -> ( Int, Int )
asEdge i j =
    if i <= j then
        ( i, j )

    else
        ( j, i )


edgeIndices : Mesh vertex -> List ( Int, Int )
edgeIndices mesh =
    let
        addFace indices edgeSet =
            case indices of
                first :: rest ->
                    List.map2 asEdge indices (rest ++ [ first ])
                        |> Set.fromList
                        |> Set.union edgeSet

                _ ->
                    edgeSet
    in
    List.foldl addFace Set.empty (faceIndices mesh) |> Set.toList


edgeVertices : Mesh vertex -> List ( vertex, vertex )
edgeVertices mesh =
    let
        toEdge ( i, j ) =
            Maybe.map2 Tuple.pair (vertex i mesh) (vertex j mesh)
    in
    List.filterMap toEdge (edgeIndices mesh)


neighbors : Mesh vertex -> Dict Int (List Int)
neighbors mesh =
    let
        insert key =
            Maybe.withDefault Set.empty
                >> Set.insert key
                >> Just

        addEdge ( i, j ) =
            Dict.update i (insert j) >> Dict.update j (insert i)
    in
    List.foldl addEdge Dict.empty (edgeIndices mesh)
        |> Dict.map (\_ s -> Set.toList s)


mapVertices : (a -> b) -> Mesh a -> Mesh b
mapVertices function (Mesh mesh) =
    Mesh
        { vertices = Array.map function mesh.vertices
        , faceIndices = mesh.faceIndices
        }


combine : List (Mesh a) -> Mesh a
combine meshes =
    let
        vertices_ =
            List.map (vertices >> Array.toList) meshes
                |> List.concat
                |> Array.fromList

        makeFaces offset meshes_ =
            case meshes_ of
                mesh :: rest ->
                    let
                        nextOffset =
                            offset + Array.length (vertices mesh)
                    in
                    List.map (List.map ((+) offset)) (faceIndices mesh)
                        ++ makeFaces nextOffset rest

                _ ->
                    []
    in
    Mesh { vertices = vertices_, faceIndices = makeFaces 0 meshes }


deduplicate : List a -> List a
deduplicate list =
    let
        unique_ seen xs =
            case xs of
                first :: rest ->
                    if List.any ((==) first) seen then
                        unique_ seen rest

                    else
                        unique_ (first :: seen) rest

                _ ->
                    List.reverse seen
    in
    unique_ [] list


indexOf : a -> List a -> Maybe Int
indexOf item list =
    let
        position_ offset xs =
            case xs of
                first :: rest ->
                    if item == first then
                        Just offset

                    else
                        position_ (offset + 1) rest

                _ ->
                    Nothing
    in
    position_ 0 list


deduplicateVertices : Mesh a -> Mesh a
deduplicateVertices mesh =
    let
        allVertices =
            vertices mesh

        uniqueVertices =
            Array.toList allVertices
                |> deduplicate

        newIndex i =
            Array.get i allVertices
                |> Maybe.andThen (\v -> indexOf v uniqueVertices)
    in
    Mesh
        { vertices = Array.fromList uniqueVertices
        , faceIndices =
            faceIndices mesh
                |> List.map (List.filterMap newIndex)
        }


triangulate : List vertex -> List ( vertex, vertex, vertex )
triangulate corners =
    case corners of
        u :: v :: rest ->
            List.map2 (\r s -> ( u, r, s )) (v :: rest) rest

        _ ->
            []


toTriangularMesh : Mesh vertex -> TriangularMesh vertex
toTriangularMesh mesh =
    TriangularMesh.indexed
        (vertices mesh)
        (faceIndices mesh |> List.concatMap triangulate)


fromTriangularMesh : TriangularMesh vertex -> Mesh vertex
fromTriangularMesh trimesh =
    Mesh
        { vertices = TriangularMesh.vertices trimesh
        , faceIndices =
            TriangularMesh.faceIndices trimesh
                |> List.map (\( u, v, w ) -> [ u, v, w ])
        }


sectorData : Array Vec3 -> List Int -> List { idx : Int, normal : Vec3 }
sectorData allVertices face =
    let
        getPos v =
            Array.get v allVertices |> Maybe.withDefault (vec3 0 0 0)

        verts =
            List.map (\v -> { idx = v, pos = getPos v }) face

        compute u v w =
            { idx = v.idx
            , normal = Vec3.cross (Vec3.sub w.pos v.pos) (Vec3.sub u.pos v.pos)
            }
    in
    case verts of
        a :: b :: rest ->
            List.map3 compute verts (b :: rest ++ [ a ]) (rest ++ [ a, b ])

        _ ->
            []


withNormals : (a -> Vec3) -> (a -> Vec3 -> b) -> Mesh a -> Mesh b
withNormals toPositionIn toVertexOut meshIn =
    let
        verticesIn =
            vertices meshIn

        positions =
            Array.map toPositionIn verticesIn

        facesIn =
            faceIndices meshIn

        allSectorData =
            List.concatMap (sectorData positions) facesIn

        convertVertex idx v =
            List.filter (\e -> e.idx == idx) allSectorData
                |> List.map .normal
                |> List.foldl Vec3.add (vec3 0 0 0)
                |> Vec3.normalize
                |> toVertexOut v
    in
    indexed (Array.indexedMap convertVertex verticesIn) facesIn


getAll : List Int -> Array a -> List a
getAll indices array =
    List.filterMap (\i -> Array.get i array) indices


centroid : List Vec3 -> Vec3
centroid points =
    List.foldl Vec3.add (vec3 0 0 0) points
        |> Vec3.scale (1 / toFloat (List.length points))


cyclicTriples : List Int -> List ( Int, Int, Int )
cyclicTriples face =
    case face of
        a :: b :: rest ->
            List.map3 (\u v w -> ( u, v, w ))
                face
                (b :: rest ++ [ a ])
                (rest ++ [ a, b ])

        _ ->
            []


subdivide :
    (vertex -> Vec3)
    -> (List vertex -> Vec3 -> vertex)
    -> Mesh vertex
    -> Mesh vertex
subdivide vertexPosition toOutputVertex meshIn =
    let
        verticesIn =
            vertices meshIn

        facesIn =
            faceIndices meshIn

        allEdges =
            edgeIndices meshIn

        nrVertices =
            Array.length verticesIn

        nrEdges =
            List.length allEdges

        midPointIndex =
            List.indexedMap (\i e -> ( e, i + nrVertices )) allEdges
                |> List.concatMap
                    (\( ( u, v ), i ) -> [ ( ( u, v ), i ), ( ( v, u ), i ) ])
                |> Dict.fromList

        makeSubFaces i f =
            cyclicTriples f
                |> List.map
                    (\( u, v, w ) ->
                        [ Dict.get ( u, v ) midPointIndex
                        , Just v
                        , Dict.get ( v, w ) midPointIndex
                        , Just (nrVertices + nrEdges + i)
                        ]
                            |> List.filterMap identity
                    )

        facesOut =
            List.indexedMap makeSubFaces facesIn
                |> List.concat

        positions =
            Array.map vertexPosition verticesIn

        makeOutputVertex indices =
            toOutputVertex
                (getAll indices verticesIn)
                (getAll indices positions |> centroid)

        verticesOut =
            [ List.range 0 (nrVertices - 1) |> List.map List.singleton
            , List.map (\( u, v ) -> [ u, v ]) allEdges
            , facesIn
            ]
                |> List.concat
                |> List.map makeOutputVertex
                |> Array.fromList
    in
    Mesh { vertices = verticesOut, faceIndices = facesOut }
