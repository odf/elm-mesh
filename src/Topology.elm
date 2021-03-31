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


type Mesh comparable
    = HalfEdgeMesh
        { next : Dict HalfEdgeKey HalfEdgeKey
        , opposite : Dict HalfEdgeKey HalfEdgeKey
        , fromVertex : Dict comparable HalfEdgeKey
        , toVertex : Dict HalfEdgeKey comparable
        , fromEdge : Dict EdgeKey HalfEdgeKey
        , toEdge : Dict HalfEdgeKey EdgeKey
        , fromFace : Dict FaceKey HalfEdgeKey
        , toFace : Dict HalfEdgeKey FaceKey
        }


type alias HalfEdgeKey =
    Int


type alias FaceKey =
    Int


type alias EdgeKey =
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


cyclicPairs : List a -> List ( a, a )
cyclicPairs indices =
    case indices of
        first :: rest ->
            List.map2 Tuple.pair indices (rest ++ [ first ])

        _ ->
            []


maybeFirst : ( Maybe a, b ) -> Maybe ( a, b )
maybeFirst p =
    case p of
        ( Just first, second ) ->
            Just ( first, second )

        _ ->
            Nothing


maybeSecond : ( a, Maybe b ) -> Maybe ( a, b )
maybeSecond p =
    case p of
        ( first, Just second ) ->
            Just ( first, second )

        _ ->
            Nothing


maybeBoth : ( Maybe a, Maybe b ) -> Maybe ( a, b )
maybeBoth p =
    case p of
        ( Just first, Just second ) ->
            Just ( first, second )

        _ ->
            Nothing


fromOrientedFaces : List (List comparable) -> Result String (Mesh comparable)
fromOrientedFaces faceLists =
    let
        orientedEdgeLists =
            List.map cyclicPairs faceLists

        orientedEdges =
            List.concat orientedEdgeLists

        toIndex =
            orientedEdges
                |> List.indexedMap (\i e -> ( e, i ))
                |> Dict.fromList

        fromIndex =
            toIndex
                |> Dict.toList
                |> List.map (\( e, i ) -> ( i, e ))
                |> Dict.fromList

        next =
            List.concatMap cyclicPairs orientedEdgeLists
                |> List.map
                    (\( a, b ) -> ( Dict.get a toIndex, Dict.get b toIndex ))
                |> List.filterMap maybeBoth
                |> Dict.fromList

        opposite =
            orientedEdges
                |> List.map (\( from, to ) -> ( ( from, to ), ( to, from ) ))
                |> List.map
                    (\( a, b ) -> ( Dict.get a toIndex, Dict.get b toIndex ))
                |> List.filterMap maybeBoth
                |> Dict.fromList

        fromVertex =
            orientedEdges
                |> List.map (\( from, to ) -> ( from, ( from, to ) ))
                |> List.map (\( a, b ) -> ( a, Dict.get b toIndex ))
                |> List.filterMap maybeSecond
                |> Dict.fromList

        toVertex =
            orientedEdges
                |> List.map (\( from, to ) -> ( ( from, to ), from ))
                |> List.map (\( a, b ) -> ( Dict.get a toIndex, b ))
                |> List.filterMap maybeFirst
                |> Dict.fromList

        fromEdge =
            orientedEdges
                |> List.filter (\( from, to ) -> from < to)
                |> List.indexedMap Tuple.pair
                |> List.map (\( a, b ) -> ( a, Dict.get b toIndex ))
                |> List.filterMap maybeSecond
                |> Dict.fromList

        toEdge =
            Dict.toList fromEdge
                |> List.concatMap
                    (\( i, e ) ->
                        [ ( Just e, i ), ( Dict.get e opposite, i ) ]
                    )
                |> List.filterMap maybeFirst
                |> Dict.fromList

        toFace =
            orientedEdgeLists
                |> List.indexedMap (\i -> List.map (\e -> ( e, i )))
                |> List.concat
                |> List.map (\( a, b ) -> ( Dict.get a toIndex, b ))
                |> List.filterMap maybeFirst
                |> Dict.fromList

        fromFace =
            Dict.toList toFace
                |> List.map (\( key, val ) -> ( val, key ))
                |> Dict.fromList

        oppositeExists e =
            Dict.get e opposite
                |> Maybe.map (\eback -> Dict.member eback toFace)
                |> Maybe.withDefault False
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
                , fromVertex = fromVertex
                , toVertex = toVertex
                , fromEdge = fromEdge
                , toEdge = toEdge
                , fromFace = fromFace
                , toFace = toFace
                }
            )


vertices : Mesh comparable -> List comparable
vertices (HalfEdgeMesh mesh) =
    Dict.keys mesh.fromVertex


halfEdgeEnds :
    HalfEdgeKey
    -> Mesh comparable
    -> Maybe ( comparable, comparable )
halfEdgeEnds edge (HalfEdgeMesh mesh) =
    let
        twin =
            Dict.get edge mesh.opposite

        from =
            Dict.get edge mesh.toVertex

        to =
            twin |> Maybe.andThen (\e -> Dict.get e mesh.toVertex)
    in
    maybeBoth ( from, to )


edges : Mesh comparable -> List ( comparable, comparable )
edges (HalfEdgeMesh mesh) =
    Dict.values mesh.fromEdge
        |> List.map (\e -> halfEdgeEnds e (HalfEdgeMesh mesh))
        |> List.filterMap identity


canonicalCircular : List comparable -> List comparable
canonicalCircular list =
    let
        argmin =
            List.indexedMap (\i k -> ( k, i )) list
                |> List.minimum
                |> Maybe.map Tuple.second
                |> Maybe.withDefault 0
    in
    List.drop argmin list ++ List.take argmin list


faceVertices : HalfEdgeKey -> Mesh comparable -> List comparable
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


vertexNeighbors : HalfEdgeKey -> Mesh comparable -> List comparable
vertexNeighbors start (HalfEdgeMesh mesh) =
    let
        step vertsIn current =
            let
                twin =
                    Dict.get current mesh.opposite

                vertsOut =
                    (twin |> Maybe.andThen (\e -> Dict.get e mesh.toVertex))
                        :: vertsIn

                advance =
                    twin |> Maybe.andThen (\e -> Dict.get e mesh.next)
            in
            case advance of
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
        originalVertices =
            vertices mesh

        vertexIndex =
            originalVertices
                |> List.indexedMap (\i v -> ( v, i ))
                |> Dict.fromList

        getAll indices dict =
            List.filterMap (\i -> Dict.get i dict) indices

        faceIndices =
            faces mesh
                |> List.map (\vs -> getAll vs vertexIndex)
                |> List.concatMap triangulate
    in
    TriangularMesh.indexed
        (originalVertices |> Array.fromList)
        faceIndices


fromTriangularMesh :
    TriangularMesh comparable
    -> Result String (Mesh comparable)
fromTriangularMesh trimesh =
    TriangularMesh.faceVertices trimesh
        |> List.map (\( u, v, w ) -> [ u, v, w ])
        |> fromOrientedFaces
