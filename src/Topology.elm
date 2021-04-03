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

import Array
import Dict exposing (Dict)
import TriangularMesh exposing (TriangularMesh)
import Tuple.Extra


type Mesh comparable
    = HalfEdgeMesh
        { next : Dict HalfEdge HalfEdge
        , opposite : Dict HalfEdge HalfEdge
        , fromVertex : Dict comparable HalfEdge
        , toVertex : Dict HalfEdge comparable
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


empty : Mesh comparable
empty =
    HalfEdgeMesh
        { next = Dict.empty
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


fromOrientedFaces : List (List comparable) -> Result String (Mesh comparable)
fromOrientedFaces faceLists =
    let
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
                { next = next
                , opposite = opposite
                , fromVertex = reverseDict toVertex
                , toVertex = toVertex
                , fromEdge = reverseDict toEdge
                , toEdge = toEdge
                , fromFace = reverseDict toFace
                , toFace = toFace
                }
            )


vertices : Mesh comparable -> List comparable
vertices (HalfEdgeMesh mesh) =
    Dict.keys mesh.fromVertex


halfEdgeEnds : HalfEdge -> Mesh comparable -> Maybe ( comparable, comparable )
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


edges : Mesh comparable -> List ( comparable, comparable )
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


faceVertices : HalfEdge -> Mesh comparable -> List comparable
faceVertices start (HalfEdgeMesh mesh) =
    let
        step vertsIn current =
            let
                vertsOut =
                    Dict.get current mesh.toVertex :: vertsIn
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
    step [] start |> List.filterMap identity |> List.reverse


faces : Mesh comparable -> List (List comparable)
faces (HalfEdgeMesh mesh) =
    Dict.values mesh.fromFace
        |> List.map (\start -> faceVertices start (HalfEdgeMesh mesh))
        |> List.map canonicalCircular


vertexNeighbors : HalfEdge -> Mesh comparable -> List comparable
vertexNeighbors start (HalfEdgeMesh mesh) =
    let
        step vertsIn current =
            let
                twin =
                    current |> lookUpIn mesh.opposite

                vertsOut =
                    (twin |> andThenLookUpIn mesh.toVertex) :: vertsIn
            in
            case twin |> andThenLookUpIn mesh.next of
                Just next ->
                    if next == start then
                        vertsOut

                    else
                        step vertsOut next

                Nothing ->
                    []
    in
    step [] start |> List.filterMap identity


neighbors : comparable -> Mesh comparable -> List comparable
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


toTriangularMesh : Mesh comparable -> TriangularMesh comparable
toTriangularMesh mesh =
    let
        meshVertices =
            vertices mesh

        vertexIndex =
            meshVertices
                |> List.indexedMap Tuple.pair
                |> List.map Tuple.Extra.flip
                |> Dict.fromList

        getIndices =
            List.filterMap (lookUpIn vertexIndex)

        faceIndices =
            faces mesh
                |> List.map getIndices
                |> List.concatMap triangulate
    in
    TriangularMesh.indexed
        (meshVertices |> Array.fromList)
        faceIndices


fromTriangularMesh :
    TriangularMesh comparable
    -> Result String (Mesh comparable)
fromTriangularMesh trimesh =
    TriangularMesh.faceVertices trimesh
        |> List.map (\( u, v, w ) -> [ u, v, w ])
        |> fromOrientedFaces
