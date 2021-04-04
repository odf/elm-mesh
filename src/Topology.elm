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
import MappedDict as Dict exposing (Dict)
import TriangularMesh exposing (TriangularMesh)
import Tuple.Extra


type Mesh vertex
    = HalfEdgeMesh
        { vertexInfo : Dict Int Vertex vertex
        , next : Dict Int HalfEdge HalfEdge
        , previous : Dict Int HalfEdge HalfEdge
        , opposite : Dict Int HalfEdge HalfEdge
        , fromVertex : Dict Int Vertex HalfEdge
        , toVertex : Dict Int HalfEdge Vertex
        , fromEdge : Dict Int Edge HalfEdge
        , toEdge : Dict Int HalfEdge Edge
        , fromFace : Dict Int Face HalfEdge
        , toFace : Dict Int HalfEdge Face
        }


type HalfEdge
    = HalfEdge Int


type Face
    = Face Int


type Edge
    = Edge Int


type Vertex
    = Vertex Int


unHalfEdge : HalfEdge -> Int
unHalfEdge (HalfEdge h) =
    h


unFace : Face -> Int
unFace (Face f) =
    f


unEdge : Edge -> Int
unEdge (Edge e) =
    e


unVertex : Vertex -> Int
unVertex (Vertex v) =
    v


empty : Mesh vertex
empty =
    HalfEdgeMesh
        { vertexInfo = Dict.empty unVertex
        , next = Dict.empty unHalfEdge
        , previous = Dict.empty unHalfEdge
        , opposite = Dict.empty unHalfEdge
        , fromVertex = Dict.empty unVertex
        , toVertex = Dict.empty unHalfEdge
        , fromEdge = Dict.empty unEdge
        , toEdge = Dict.empty unHalfEdge
        , fromFace = Dict.empty unFace
        , toFace = Dict.empty unHalfEdge
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


fromOrientedFaces :
    Array vertex
    -> List (List Int)
    -> Result String (Mesh vertex)
fromOrientedFaces vertexData faceLists =
    let
        vertexInfo =
            Array.toList vertexData
                |> List.indexedMap (\n d -> ( Vertex n, d ))
                |> Dict.fromList unVertex

        orientedEdgeLists =
            List.map cyclicPairs faceLists

        orientedEdges =
            List.concat orientedEdgeLists

        toKey =
            orientedEdges
                |> List.indexedMap (\i h -> ( h, i ))
                |> Dict.fromList identity

        getKey =
            Dict.getIn toKey

        betweenKeyed =
            List.map (Tuple.Extra.map getKey)
                >> List.filterMap Tuple.Extra.sequenceMaybe
                >> List.map (Tuple.Extra.map HalfEdge)
                >> Dict.fromList unHalfEdge

        fromKeyed =
            List.map (Tuple.mapFirst getKey)
                >> List.filterMap Tuple.Extra.sequenceFirstMaybe
                >> List.map (Tuple.mapFirst HalfEdge)
                >> Dict.fromList unHalfEdge

        next =
            List.concatMap cyclicPairs orientedEdgeLists
                |> betweenKeyed

        opposite =
            orientedEdges
                |> List.map (\( from, to ) -> ( ( from, to ), ( to, from ) ))
                |> betweenKeyed

        toVertex =
            orientedEdges
                |> List.map (\( from, to ) -> ( ( from, to ), Vertex from ))
                |> fromKeyed

        toEdge =
            orientedEdges
                |> List.filter (\( from, to ) -> from < to)
                |> List.indexedMap Tuple.pair
                |> List.concatMap
                    (\( i, ( from, to ) ) ->
                        [ ( ( from, to ), Edge i ), ( ( to, from ), Edge i ) ]
                    )
                |> fromKeyed

        toFace =
            orientedEdgeLists
                |> List.indexedMap (\i -> List.map (\h -> ( h, Face i )))
                |> List.concat
                |> fromKeyed

        oppositeExists =
            Dict.getIn opposite
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
                , previous = Dict.reverse unHalfEdge next
                , opposite = opposite
                , fromVertex = Dict.reverse unVertex toVertex
                , toVertex = toVertex
                , fromEdge = Dict.reverse unEdge toEdge
                , toEdge = toEdge
                , fromFace = Dict.reverse unFace toFace
                , toFace = toFace
                }
            )


vertices : Mesh vertex -> List vertex
vertices (HalfEdgeMesh mesh) =
    Dict.values mesh.vertexInfo


halfEdgeEnds : HalfEdge -> Mesh vertex -> Maybe ( Vertex, Vertex )
halfEdgeEnds edge (HalfEdgeMesh mesh) =
    let
        from =
            edge
                |> Dict.getIn mesh.toVertex

        to =
            edge
                |> Dict.getIn mesh.opposite
                |> Dict.andThenGetIn mesh.toVertex
    in
    Tuple.Extra.sequenceMaybe ( from, to )


edges : Mesh vertex -> List ( Int, Int )
edges (HalfEdgeMesh mesh) =
    Dict.values mesh.fromEdge
        |> List.map (\e -> halfEdgeEnds e (HalfEdgeMesh mesh))
        |> List.filterMap identity
        |> List.map (Tuple.Extra.map unVertex)
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
            case current |> Dict.getIn mesh.previous of
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
        |> List.filterMap (Dict.getIn mesh.toVertex)


faces : Mesh vertex -> List (List Int)
faces (HalfEdgeMesh mesh) =
    Dict.values mesh.fromFace
        |> List.map (\start -> faceVertices start (HalfEdgeMesh mesh))
        |> List.map (List.map unVertex >> canonicalCircular)


vertexRange : HalfEdge -> HalfEdge -> Mesh vertex -> List HalfEdge
vertexRange start end (HalfEdgeMesh mesh) =
    let
        step current tail =
            case
                current
                    |> Dict.getIn mesh.opposite
                    |> Dict.andThenGetIn mesh.next
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


vertexNeighbors : HalfEdge -> Mesh vertex -> List Vertex
vertexNeighbors start (HalfEdgeMesh mesh) =
    vertexRange start start (HalfEdgeMesh mesh)
        |> List.filterMap
            (Dict.getIn mesh.opposite
                >> Dict.andThenGetIn mesh.toVertex
            )


neighbors : Int -> Mesh vertex -> List Int
neighbors vertex (HalfEdgeMesh mesh) =
    Dict.get (Vertex vertex) mesh.fromVertex
        |> Maybe.map (\start -> vertexNeighbors start (HalfEdgeMesh mesh))
        |> Maybe.withDefault []
        |> List.map unVertex
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
                |> List.indexedMap (\i (Vertex v) -> ( v, i ))
                |> Dict.fromList identity

        getIndices =
            List.filterMap (Dict.getIn vertexIndex)

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
