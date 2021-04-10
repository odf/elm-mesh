module Mesh.Topology exposing
    ( Mesh
    , edgeIndices
    , edgeVertices
    , empty
    , faceIndices
    , faceVertices
    , fromOrientedFaces
    , fromTriangularMesh
    , neighborIndices
    , toTriangularMesh
    , vertices
    )

import Array exposing (Array)
import Dict exposing (Dict)
import TriangularMesh exposing (TriangularMesh)


type Mesh vertex
    = Mesh
        { vertices : Array vertex
        , atVertex : Array OrientedEdge
        , alongFace : Array OrientedEdge
        , next : Dict OrientedEdge OrientedEdge
        , toFace : Dict OrientedEdge Int
        }


type alias OrientedEdge =
    ( Int, Int )


opposite : OrientedEdge -> OrientedEdge
opposite ( from, to ) =
    ( to, from )


empty : Mesh vertex
empty =
    Mesh
        { vertices = Array.empty
        , atVertex = Array.empty
        , alongFace = Array.empty
        , next = Dict.empty
        , toFace = Dict.empty
        }


fromOrientedFaces :
    Array vertex
    -> List (List Int)
    -> Result String (Mesh vertex)
fromOrientedFaces vertexData faceLists =
    let
        orientedEdgeLists =
            List.map cyclicPairs faceLists

        orientedEdges =
            List.concat orientedEdgeLists

        next =
            List.concatMap cyclicPairs orientedEdgeLists |> Dict.fromList

        fromVertex =
            orientedEdges
                |> List.map (\( from, to ) -> ( from, ( from, to ) ))
                |> Dict.fromList
                |> Dict.values
                |> Array.fromList

        toFace =
            orientedEdgeLists
                |> List.indexedMap (\i -> List.map (\e -> ( e, i )))
                |> List.concat
                |> Dict.fromList

        fromFace =
            Dict.toList toFace
                |> List.map (\( key, val ) -> ( val, key ))
                |> Dict.fromList
                |> Dict.values
                |> Array.fromList

        seen e =
            Dict.member e toFace
    in
    if findDuplicate orientedEdges /= Nothing then
        Err "each oriented edge must be unique"

    else if List.any (opposite >> seen >> not) orientedEdges then
        Err "each oriented edge must have a reverse"

    else
        Ok
            (Mesh
                { vertices = vertexData
                , atVertex = fromVertex
                , alongFace = fromFace
                , next = next
                , toFace = toFace
                }
            )


vertices : Mesh vertex -> Array vertex
vertices (Mesh mesh) =
    mesh.vertices


vertex : Int -> Mesh vertex -> Maybe vertex
vertex index mesh =
    Array.get index (vertices mesh)


verticesInFace : OrientedEdge -> Mesh vertex -> List Int
verticesInFace start (Mesh mesh) =
    let
        step vertsIn current =
            let
                vertsOut =
                    Tuple.first current :: vertsIn
            in
            case Dict.get current mesh.next of
                Just next ->
                    if next == start then
                        vertsOut

                    else
                        step vertsOut next

                Nothing ->
                    []
    in
    step [] start |> List.reverse


faceIndices : Mesh vertex -> List (List Int)
faceIndices (Mesh mesh) =
    Array.toList mesh.alongFace
        |> List.map (\start -> verticesInFace start (Mesh mesh))
        |> List.map canonicalCircular


faceVertices : Mesh vertex -> List (List vertex)
faceVertices mesh =
    let
        toFace =
            List.filterMap (\i -> vertex i mesh)
    in
    List.map toFace (faceIndices mesh)


edgeIndices : Mesh vertex -> List ( Int, Int )
edgeIndices (Mesh mesh) =
    Dict.keys mesh.next |> List.filter (\( from, to ) -> from <= to)


edgeVertices : Mesh vertex -> List ( vertex, vertex )
edgeVertices mesh =
    let
        toEdge ( i, j ) =
            Maybe.map2 Tuple.pair (vertex i mesh) (vertex j mesh)
    in
    List.filterMap toEdge (edgeIndices mesh)


vertexNeighbors : OrientedEdge -> Mesh vertex -> List Int
vertexNeighbors start (Mesh mesh) =
    let
        step vertsIn current =
            let
                vertsOut =
                    Tuple.second current :: vertsIn
            in
            case Dict.get (opposite current) mesh.next of
                Just next ->
                    if next == start then
                        vertsOut

                    else
                        step vertsOut next

                Nothing ->
                    []
    in
    step [] start


neighborIndices : Int -> Mesh vertex -> List Int
neighborIndices vertexIndex (Mesh mesh) =
    Array.get vertexIndex mesh.atVertex
        |> Maybe.map (\start -> vertexNeighbors start (Mesh mesh))
        |> Maybe.withDefault []
        |> canonicalCircular


toTriangularMesh : Mesh vertex -> TriangularMesh vertex
toTriangularMesh mesh =
    TriangularMesh.indexed
        (vertices mesh)
        (faceIndices mesh |> List.concatMap triangulate)


fromTriangularMesh : TriangularMesh vertex -> Result String (Mesh vertex)
fromTriangularMesh trimesh =
    TriangularMesh.faceIndices trimesh
        |> List.map (\( u, v, w ) -> [ u, v, w ])
        |> fromOrientedFaces (TriangularMesh.vertices trimesh)



-- List helpers


triangulate : List vertex -> List ( vertex, vertex, vertex )
triangulate corners =
    case corners of
        u :: v :: rest ->
            List.map2 (\r s -> ( u, r, s )) (v :: rest) rest

        _ ->
            []


findDuplicate : List comparable -> Maybe comparable
findDuplicate =
    let
        findDupe list =
            case list of
                first :: second :: rest ->
                    if first == second then
                        Just first

                    else
                        findDupe (second :: rest)

                _ ->
                    Nothing
    in
    List.sort >> findDupe


cyclicPairs : List a -> List ( a, a )
cyclicPairs indices =
    case indices of
        first :: rest ->
            List.map2 Tuple.pair indices (rest ++ [ first ])

        _ ->
            []


canonicalCircular : List comparable -> List comparable
canonicalCircular list =
    List.range 0 (List.length list - 1)
        |> List.map (\k -> List.drop k list ++ List.take k list)
        |> List.minimum
        |> Maybe.withDefault list
