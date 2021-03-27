module Topology exposing
    ( Mesh
    , empty
    )

import Array exposing (Array)
import Dict
import Mesh exposing (vertices)
import Set


type HalfEdge
    = HalfEdge
        { twin : Int
        , next : Int
        , vertex : Vertex
        , edge : Edge
        , face : Face
        }


type Vertex
    = Vertex HalfEdge


type Edge
    = Edge HalfEdge


type Face
    = Face HalfEdge


type Mesh
    = Mesh
        { halfEdges : Array HalfEdge
        , vertices : Array Vertex
        , edges : Array Edge
        , faces : Array Face
        }


empty : Mesh
empty =
    Mesh
        { halfEdges = Array.empty
        , vertices = Array.empty
        , edges = Array.empty
        , faces = Array.empty
        }


hasDuplicates : List comparable -> Bool
hasDuplicates =
    let
        hasDupes list =
            case list of
                a :: b :: rest ->
                    a == b || hasDupes (b :: rest)

                _ ->
                    False
    in
    List.sort >> hasDupes


fromOrientedFaces : List (List Int) -> Result String Mesh
fromOrientedFaces faces =
    let
        vertices =
            List.concat faces |> Set.fromList |> Set.toList

        cyclicPairs indices =
            case indices of
                a :: rest ->
                    List.map2 Tuple.pair indices (rest ++ [ a ])

                _ ->
                    []

        halfEdgeLists =
            List.map cyclicPairs faces

        halfEdgeOrder =
            List.concat halfEdgeLists

        halfEdgeIndices =
            List.indexedMap (\i he -> ( he, i )) halfEdgeOrder
                |> Dict.fromList

        isManifold =
            List.all
                (\( a, b ) -> Dict.member ( b, a ) halfEdgeIndices)
                halfEdgeOrder

        nextHalfEdge =
            List.concatMap cyclicPairs halfEdgeLists
                |> Dict.fromList
    in
    if vertices /= List.range 0 (List.length vertices - 1) then
        Err "vertex numbers must be consecutive, starting at 0"

    else if hasDuplicates halfEdgeLists then
        Err "each oriented edge must be unique"

    else if not isManifold then
        Err "each oriented edge must have a reverse"

    else
        Err "not yet implemented"
