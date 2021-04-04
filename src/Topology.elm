module Topology exposing
    ( Mesh
    , edges
    , empty
    , faces
    , fromOrientedFaces
    , fromTriangularMesh
    , neighbors
    , toTriangularMesh
    , vertices
    )

import Array exposing (Array)
import Dict exposing (Dict)
import TriangularMesh exposing (TriangularMesh)
import Tuple.Extra


type Mesh vertex
    = HalfEdgeMesh
        { vertexInfo : Dict Vertex vertex
        , next : Dict HalfEdge HalfEdge
        , previous : Dict HalfEdge HalfEdge
        , opposite : Dict HalfEdge HalfEdge
        , fromVertex : Dict Vertex HalfEdge
        , toVertex : Dict HalfEdge Vertex
        , fromEdge : Dict Edge HalfEdge
        , toEdge : Dict HalfEdge Edge
        , fromFace : Dict Face HalfEdge
        , toFace : Dict HalfEdge Face
        }


type alias HalfEdge =
    Int


type alias Face =
    Int


type alias Edge =
    Int


type alias Vertex =
    Int


empty : Mesh vertex
empty =
    HalfEdgeMesh
        { vertexInfo = Dict.empty
        , next = Dict.empty
        , previous = Dict.empty
        , opposite = Dict.empty
        , fromVertex = Dict.empty
        , toVertex = Dict.empty
        , fromEdge = Dict.empty
        , toEdge = Dict.empty
        , fromFace = Dict.empty
        , toFace = Dict.empty
        }


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


pairs : List a -> List ( a, a )
pairs xs =
    case xs of
        first :: second :: rest ->
            ( first, second ) :: pairs (second :: rest)

        _ ->
            []


cyclicPairs : List a -> List ( a, a )
cyclicPairs xs =
    case xs of
        first :: _ ->
            pairs (xs ++ [ first ])

        _ ->
            []


lookUpIn : Dict comparable b -> comparable -> Maybe b
lookUpIn dict key =
    Dict.get key dict


andThenLookUpIn : Dict comparable b -> Maybe comparable -> Maybe b
andThenLookUpIn =
    lookUpIn >> Maybe.andThen


reverseDict : Dict comparable1 comparable2 -> Dict comparable2 comparable1
reverseDict =
    Dict.toList >> List.map Tuple.Extra.flip >> Dict.fromList


fromOrientedFaces :
    Array vertex
    -> List (List Int)
    -> Result String (Mesh vertex)
fromOrientedFaces vertexData faceLists =
    let
        vertexInfo =
            Array.toList vertexData
                |> List.indexedMap Tuple.pair
                |> Dict.fromList

        orientedEdgeLists =
            List.map cyclicPairs faceLists

        orientedEdges =
            List.concat orientedEdgeLists

        toKey =
            orientedEdges
                |> List.indexedMap Tuple.pair
                |> List.map Tuple.Extra.flip
                |> Dict.fromList

        getKey =
            lookUpIn toKey

        betweenKeyed =
            List.map (Tuple.Extra.map getKey)
                >> List.filterMap Tuple.Extra.sequenceMaybe
                >> Dict.fromList

        fromKeyed =
            List.map (Tuple.mapFirst getKey)
                >> List.filterMap Tuple.Extra.sequenceFirstMaybe
                >> Dict.fromList

        next =
            List.concatMap cyclicPairs orientedEdgeLists
                |> betweenKeyed

        opposite =
            orientedEdges
                |> List.map (\( from, to ) -> ( ( from, to ), ( to, from ) ))
                |> betweenKeyed

        toVertex =
            orientedEdges
                |> List.map (\( from, to ) -> ( ( from, to ), from ))
                |> fromKeyed

        toEdge =
            orientedEdges
                |> List.filter (\( from, to ) -> from < to)
                |> List.indexedMap Tuple.pair
                |> List.concatMap
                    (\( i, ( from, to ) ) ->
                        [ ( ( from, to ), i ), ( ( to, from ), i ) ]
                    )
                |> fromKeyed

        toFace =
            orientedEdgeLists
                |> List.indexedMap (Tuple.Extra.pairWith >> List.map)
                |> List.concat
                |> fromKeyed

        oppositeExists =
            lookUpIn opposite
                >> Maybe.map (\opp -> Dict.member opp toFace)
                >> Maybe.withDefault False
    in
    if findDuplicate orientedEdges /= Nothing then
        Err "each oriented edge must be unique"

    else if List.any (oppositeExists >> not) (Dict.keys next) then
        Err "each oriented edge must have a reverse"

    else
        Ok
            (HalfEdgeMesh
                { vertexInfo = vertexInfo
                , next = next
                , previous = reverseDict next
                , opposite = opposite
                , fromVertex = reverseDict toVertex
                , toVertex = toVertex
                , fromEdge = reverseDict toEdge
                , toEdge = toEdge
                , fromFace = reverseDict toFace
                , toFace = toFace
                }
            )


vertices : Mesh vertex -> List vertex
vertices (HalfEdgeMesh mesh) =
    Dict.values mesh.vertexInfo


halfEdgeEnds : HalfEdge -> Mesh vertex -> Maybe ( Int, Int )
halfEdgeEnds edge (HalfEdgeMesh mesh) =
    let
        from =
            edge
                |> lookUpIn mesh.toVertex

        to =
            edge
                |> lookUpIn mesh.opposite
                |> andThenLookUpIn mesh.toVertex
    in
    Tuple.Extra.sequenceMaybe ( from, to )


edges : Mesh vertex -> List ( Int, Int )
edges (HalfEdgeMesh mesh) =
    Dict.values mesh.fromEdge
        |> List.map (\e -> halfEdgeEnds e (HalfEdgeMesh mesh))
        |> List.filterMap identity
        |> List.map (\( from, to ) -> ( min from to, max from to ))


canonicalCircular : List comparable -> List comparable
canonicalCircular list =
    List.range 0 (List.length list - 1)
        |> List.map (\i -> List.drop i list ++ List.take i list)
        |> List.minimum
        |> Maybe.withDefault list


faceRange : HalfEdge -> HalfEdge -> Mesh vertex -> List HalfEdge
faceRange start end (HalfEdgeMesh mesh) =
    let
        step current tail =
            case current |> lookUpIn mesh.previous of
                Just previous ->
                    if previous == start then
                        current :: tail

                    else
                        step previous (current :: tail)

                Nothing ->
                    []
    in
    step end []


faceVertices : HalfEdge -> Mesh vertex -> List Vertex
faceVertices start (HalfEdgeMesh mesh) =
    faceRange start start (HalfEdgeMesh mesh)
        |> List.filterMap (lookUpIn mesh.toVertex)


faces : Mesh vertex -> List (List Int)
faces (HalfEdgeMesh mesh) =
    Dict.values mesh.fromFace
        |> List.map (\start -> faceVertices start (HalfEdgeMesh mesh))
        |> List.map canonicalCircular


vertexRange : HalfEdge -> HalfEdge -> Mesh vertex -> List HalfEdge
vertexRange start end (HalfEdgeMesh mesh) =
    let
        step current tail =
            case
                current
                    |> lookUpIn mesh.opposite
                    |> andThenLookUpIn mesh.next
            of
                Just previous ->
                    if previous == start then
                        current :: tail

                    else
                        step previous (current :: tail)

                Nothing ->
                    []
    in
    step end []


vertexNeighbors : HalfEdge -> Mesh vertex -> List Int
vertexNeighbors start (HalfEdgeMesh mesh) =
    vertexRange start start (HalfEdgeMesh mesh)
        |> List.filterMap
            (lookUpIn mesh.opposite
                >> andThenLookUpIn mesh.toVertex
            )


neighbors : Int -> Mesh vertex -> List Int
neighbors vertex (HalfEdgeMesh mesh) =
    Dict.get vertex mesh.fromVertex
        |> Maybe.map (\start -> vertexNeighbors start (HalfEdgeMesh mesh))
        |> Maybe.withDefault []
        |> canonicalCircular


triangulate : List vertex -> List ( vertex, vertex, vertex )
triangulate corners =
    case corners of
        u :: v :: rest ->
            List.map2 (\r s -> ( u, r, s )) (v :: rest) rest

        _ ->
            []


toTriangularMesh : Mesh vertex -> TriangularMesh vertex
toTriangularMesh (HalfEdgeMesh mesh) =
    let
        vertexIndex =
            Dict.keys mesh.vertexInfo
                |> List.indexedMap Tuple.pair
                |> List.map Tuple.Extra.flip
                |> Dict.fromList

        getIndices =
            List.filterMap (lookUpIn vertexIndex)

        faceIndices =
            faces (HalfEdgeMesh mesh)
                |> List.map getIndices
                |> List.concatMap triangulate
    in
    TriangularMesh.indexed
        (Dict.values mesh.vertexInfo |> Array.fromList)
        faceIndices


fromTriangularMesh : TriangularMesh vertex -> Result String (Mesh vertex)
fromTriangularMesh trimesh =
    TriangularMesh.faceIndices trimesh
        |> List.map (\( u, v, w ) -> [ u, v, w ])
        |> fromOrientedFaces (TriangularMesh.vertices trimesh)
