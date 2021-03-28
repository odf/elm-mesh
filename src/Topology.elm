module Topology exposing
    ( Mesh
    , empty
    , fromOrientedFaces
    )

import Dict exposing (Dict)


type Mesh comparable
    = HalfEdgeMesh
        { next : Dict (DirectedEdge comparable) (DirectedEdge comparable)
        , fromVertex : Dict comparable (DirectedEdge comparable)
        , fromFace : Dict FaceKey (DirectedEdge comparable)
        , toFace : Dict (DirectedEdge comparable) FaceKey
        }


type alias DirectedEdge comparable =
    ( comparable, comparable )


type alias FaceKey =
    Int


opposite : DirectedEdge comparable -> DirectedEdge comparable
opposite ( from, to ) =
    ( to, from )


edge : DirectedEdge comparable -> ( comparable, comparable )
edge ( from, to ) =
    if from <= to then
        ( from, to )

    else
        ( to, from )


startVertex : DirectedEdge comparable -> comparable
startVertex ( from, _ ) =
    from


endVertex : DirectedEdge comparable -> comparable
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
fromOrientedFaces faces =
    let
        directedEdgeLists =
            List.map cyclicPairs faces

        directedEdges =
            List.concat directedEdgeLists

        next =
            List.concatMap cyclicPairs directedEdgeLists |> Dict.fromList

        fromVertex =
            directedEdges
                |> List.map (\( from, to ) -> ( from, ( from, to ) ))
                |> Dict.fromList

        toFace =
            directedEdgeLists
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
    if findDuplicate directedEdges /= Nothing then
        Err "each oriented edge must be unique"

    else if List.any (opposite >> seen >> not) directedEdges then
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
