module Mesh exposing
    ( Mesh
    , combine
    , edgeIndices
    , edgeVertices
    , empty
    , faceIndices
    , faceVertices
    , faces
    , indexed
    , mapVertices
    , vertex
    , vertices
    )

import Array exposing (Array)
import Set


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
