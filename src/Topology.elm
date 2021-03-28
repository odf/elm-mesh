module Topology exposing
    ( Mesh
    , edges
    , empty
    , faces
    , fromOrientedFaces
    , neighbors
    , vertices
    )

import Dict exposing (Dict)


type Mesh comparable
    = HalfEdgeMesh
        { next : Dict (OrientedEdge comparable) (OrientedEdge comparable)
        , fromVertex : Dict comparable (OrientedEdge comparable)
        , fromFace : Dict FaceKey (OrientedEdge comparable)
        , toFace : Dict (OrientedEdge comparable) FaceKey
        }


type alias OrientedEdge comparable =
    ( comparable, comparable )


type alias FaceKey =
    Int


opposite : OrientedEdge comparable -> OrientedEdge comparable
opposite ( from, to ) =
    ( to, from )


edge : OrientedEdge comparable -> ( comparable, comparable )
edge ( from, to ) =
    if from <= to then
        ( from, to )

    else
        ( to, from )


startVertex : OrientedEdge comparable -> comparable
startVertex ( from, _ ) =
    from


endVertex : OrientedEdge comparable -> comparable
endVertex ( _, to ) =
    to


empty : Mesh comparable
empty =
    HalfEdgeMesh
        { next = Dict.empty
        , fromVertex = Dict.empty
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


fromOrientedFaces : List (List comparable) -> Result String (Mesh comparable)
fromOrientedFaces faceLists =
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

        toFace =
            orientedEdgeLists
                |> List.indexedMap (\i -> List.map (\e -> ( e, i )))
                |> List.concat
                |> Dict.fromList

        fromFace =
            Dict.toList toFace
                |> List.map (\( key, val ) -> ( val, key ))
                |> Dict.fromList

        seen e =
            Dict.member e toFace
    in
    if findDuplicate orientedEdges /= Nothing then
        Err "each oriented edge must be unique"

    else if List.any (opposite >> seen >> not) orientedEdges then
        Err "each oriented edge must have a reverse"

    else
        Ok
            (HalfEdgeMesh
                { next = next
                , fromVertex = fromVertex
                , fromFace = fromFace
                , toFace = toFace
                }
            )


vertices : Mesh comparable -> List comparable
vertices (HalfEdgeMesh mesh) =
    Dict.keys mesh.fromVertex


edges : Mesh comparable -> List ( comparable, comparable )
edges (HalfEdgeMesh mesh) =
    Dict.keys mesh.next |> List.filter (\( from, to ) -> from <= to)


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


faceVertices : OrientedEdge comparable -> Mesh comparable -> List comparable
faceVertices start (HalfEdgeMesh mesh) =
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


faces : Mesh comparable -> List (List comparable)
faces (HalfEdgeMesh mesh) =
    Dict.values mesh.fromFace
        |> List.map (\start -> faceVertices start (HalfEdgeMesh mesh))
        |> List.map canonicalCircular


vertexNeighbors : OrientedEdge comparable -> Mesh comparable -> List comparable
vertexNeighbors start (HalfEdgeMesh mesh) =
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


neighbors : comparable -> Mesh comparable -> List comparable
neighbors vertex (HalfEdgeMesh mesh) =
    Dict.get vertex mesh.fromVertex
        |> Maybe.map (\start -> vertexNeighbors start (HalfEdgeMesh mesh))
        |> Maybe.withDefault []
        |> canonicalCircular
