module Mesh exposing
    ( Mesh
    , combine
    , edgeIndices
    , edgeVertices
    , empty
    , faceIndices
    , faceVertices
    , faces
    , fromTriangularMesh
    , indexed
    , joinVertices
    , mapVertices
    , toTriangularMesh
    , vertex
    , vertices
    )

import Array exposing (Array)
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


unique : List a -> List a
unique list =
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


position : a -> List a -> Maybe Int
position item list =
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


joinVertices : Mesh a -> Mesh a
joinVertices mesh =
    let
        allVertices =
            vertices mesh

        uniqueVertices =
            Array.toList allVertices
                |> unique

        newIndex i =
            Array.get i allVertices
                |> Maybe.andThen (\v -> position v uniqueVertices)
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
