module TopologyTests exposing
    ( combine
    , empty
    , fromTriangular
    , goodFaceList
    , mapVertices
    , orientationMismatch
    , toTriangular
    , unpairedOrientedEdge
    )

import Array exposing (Array)
import Expect
import Mesh.Topology as Topology
import Test exposing (Test)
import TriangularMesh


octahedronVertices : Array String
octahedronVertices =
    Array.fromList [ "front", "right", "top", "back", "left", "bottom" ]


octahedronFaces : List (List Int)
octahedronFaces =
    [ [ 0, 1, 2 ]
    , [ 1, 0, 5 ]
    , [ 2, 1, 3 ]
    , [ 0, 2, 4 ]
    , [ 3, 5, 4 ]
    , [ 5, 3, 1 ]
    , [ 4, 5, 0 ]
    , [ 3, 4, 2 ]
    ]


empty : Test
empty =
    Test.test "empty"
        (\() ->
            Topology.empty
                |> Expect.all
                    [ Topology.vertices >> Array.toList >> Expect.equal []
                    , Topology.edgeIndices >> Expect.equal []
                    , Topology.faceIndices >> Expect.equal []
                    ]
        )


goodFaceList : Test
goodFaceList =
    Test.test "goodFaceList"
        (\() ->
            octahedronFaces
                |> Topology.fromOrientedFaces octahedronVertices
                |> Result.withDefault Topology.empty
                |> Expect.all
                    [ Topology.vertices
                        >> Expect.equal octahedronVertices
                    , Topology.edgeIndices
                        >> Expect.equal
                            [ ( 0, 1 )
                            , ( 0, 2 )
                            , ( 0, 4 )
                            , ( 0, 5 )
                            , ( 1, 2 )
                            , ( 1, 3 )
                            , ( 1, 5 )
                            , ( 2, 3 )
                            , ( 2, 4 )
                            , ( 3, 4 )
                            , ( 3, 5 )
                            , ( 4, 5 )
                            ]
                    , Topology.edgeVertices
                        >> Expect.equal
                            [ ( "front", "right" )
                            , ( "front", "top" )
                            , ( "front", "left" )
                            , ( "front", "bottom" )
                            , ( "right", "top" )
                            , ( "right", "back" )
                            , ( "right", "bottom" )
                            , ( "top", "back" )
                            , ( "top", "left" )
                            , ( "back", "left" )
                            , ( "back", "bottom" )
                            , ( "left", "bottom" )
                            ]
                    , Topology.faceIndices
                        >> List.sort
                        >> Expect.equal
                            [ [ 0, 1, 2 ]
                            , [ 0, 2, 4 ]
                            , [ 0, 4, 5 ]
                            , [ 0, 5, 1 ]
                            , [ 1, 3, 2 ]
                            , [ 1, 5, 3 ]
                            , [ 2, 3, 4 ]
                            , [ 3, 5, 4 ]
                            ]
                    , Topology.faceVertices
                        >> List.sort
                        >> Expect.equal
                            [ [ "back", "bottom", "left" ]
                            , [ "front", "bottom", "right" ]
                            , [ "front", "left", "bottom" ]
                            , [ "front", "right", "top" ]
                            , [ "front", "top", "left" ]
                            , [ "right", "back", "top" ]
                            , [ "right", "bottom", "back" ]
                            , [ "top", "back", "left" ]
                            ]
                    , Topology.neighborIndices
                        >> Array.toList
                        >> Expect.equal
                            [ [ 1, 2, 4, 5 ]
                            , [ 0, 5, 3, 2 ]
                            , [ 0, 1, 3, 4 ]
                            , [ 1, 5, 4, 2 ]
                            , [ 0, 2, 3, 5 ]
                            , [ 0, 4, 3, 1 ]
                            ]
                    , Topology.neighborVertices
                        >> Array.toList
                        >> Expect.equal
                            [ [ "right", "top", "left", "bottom" ]
                            , [ "front", "bottom", "back", "top" ]
                            , [ "front", "right", "back", "left" ]
                            , [ "right", "bottom", "left", "top" ]
                            , [ "front", "top", "back", "bottom" ]
                            , [ "front", "left", "back", "right" ]
                            ]
                    ]
        )


unpairedOrientedEdge : Test
unpairedOrientedEdge =
    Test.test "unpairedOrientedEdge"
        (\() ->
            octahedronFaces
                |> List.drop 1
                |> Topology.fromOrientedFaces octahedronVertices
                |> Expect.err
        )


orientationMismatch : Test
orientationMismatch =
    Test.test "orientationMismatch"
        (\() ->
            octahedronFaces
                |> List.drop 1
                |> (::) [ 0, 2, 1 ]
                |> Topology.fromOrientedFaces octahedronVertices
                |> Expect.err
        )


toTriangular : Test
toTriangular =
    Test.test "toTriangular"
        (\() ->
            [ [ 0, 1, 2, 3 ], [ 3, 2, 1, 0 ] ]
                |> Topology.fromOrientedFaces
                    (Array.fromList [ 'a', 'b', 'c', 'd' ])
                |> Result.withDefault Topology.empty
                |> Topology.toTriangularMesh
                |> Expect.all
                    [ TriangularMesh.vertices
                        >> Array.toList
                        >> Expect.equal [ 'a', 'b', 'c', 'd' ]
                    , TriangularMesh.faceIndices
                        >> Expect.equal
                            [ ( 0, 1, 2 )
                            , ( 0, 2, 3 )
                            , ( 0, 3, 2 )
                            , ( 0, 2, 1 )
                            ]
                    , TriangularMesh.faceVertices
                        >> Expect.equal
                            [ ( 'a', 'b', 'c' )
                            , ( 'a', 'c', 'd' )
                            , ( 'a', 'd', 'c' )
                            , ( 'a', 'c', 'b' )
                            ]
                    ]
        )


fromTriangular : Test
fromTriangular =
    Test.test "fromTriangular"
        (\() ->
            TriangularMesh.indexed
                (Array.fromList [ 'a', 'b', 'c', 'd', 'e', 'f' ])
                [ ( 0, 1, 2 )
                , ( 1, 0, 5 )
                , ( 2, 1, 3 )
                , ( 0, 2, 4 )
                , ( 3, 5, 4 )
                , ( 5, 3, 1 )
                , ( 4, 5, 0 )
                , ( 3, 4, 2 )
                ]
                |> Topology.fromTriangularMesh
                |> Result.withDefault Topology.empty
                |> Expect.all
                    [ Topology.vertices
                        >> Array.toList
                        >> Expect.equal [ 'a', 'b', 'c', 'd', 'e', 'f' ]
                    , Topology.faceIndices
                        >> List.sort
                        >> Expect.equal
                            [ [ 0, 1, 2 ]
                            , [ 0, 2, 4 ]
                            , [ 0, 4, 5 ]
                            , [ 0, 5, 1 ]
                            , [ 1, 3, 2 ]
                            , [ 1, 5, 3 ]
                            , [ 2, 3, 4 ]
                            , [ 3, 5, 4 ]
                            ]
                    , Topology.faceVertices
                        >> List.sort
                        >> Expect.equal
                            [ [ 'a', 'b', 'c' ]
                            , [ 'a', 'c', 'e' ]
                            , [ 'a', 'e', 'f' ]
                            , [ 'a', 'f', 'b' ]
                            , [ 'b', 'd', 'c' ]
                            , [ 'b', 'f', 'd' ]
                            , [ 'c', 'd', 'e' ]
                            , [ 'd', 'f', 'e' ]
                            ]
                    ]
        )


mapVertices : Test
mapVertices =
    Test.test "mapVertices"
        (\() ->
            Topology.fromOrientedFaces octahedronVertices octahedronFaces
                |> Result.withDefault Topology.empty
                |> Topology.mapVertices (String.slice 0 2 >> String.toUpper)
                |> Expect.all
                    [ Topology.vertices
                        >> Array.toList
                        >> Expect.equal [ "FR", "RI", "TO", "BA", "LE", "BO" ]
                    ]
        )


combine : Test
combine =
    let
        tetra =
            Topology.fromOrientedFaces
                (Array.fromList [ 0, 1, 2, 3 ])
                [ [ 0, 1, 2 ], [ 0, 2, 3 ], [ 0, 3, 1 ], [ 3, 2, 1 ] ]
                |> Result.withDefault Topology.empty
    in
    Test.test "combine"
        (\() ->
            Topology.combine [ tetra, Topology.mapVertices ((+) 4) tetra ]
                |> Expect.all
                    [ Topology.vertices
                        >> Array.toList
                        >> Expect.equal [ 0, 1, 2, 3, 4, 5, 6, 7 ]
                    , Topology.faceIndices
                        >> Expect.equal
                            [ [ 0, 1, 2 ]
                            , [ 0, 2, 3 ]
                            , [ 0, 3, 1 ]
                            , [ 1, 3, 2 ]
                            , [ 4, 5, 6 ]
                            , [ 4, 6, 7 ]
                            , [ 4, 7, 5 ]
                            , [ 5, 7, 6 ]
                            ]
                    ]
        )
