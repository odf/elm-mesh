module TopologyTests exposing
    ( empty
    , fromTriangular
    , goodFaceList
    , orientationMismatch
    , toTriangular
    , unpairedOrientedEdge
    )

import Array exposing (Array)
import Expect
import Test exposing (Test)
import Topology
import TriangularMesh


octahedronVertices : Array Int
octahedronVertices =
    [ 0, 1, 2, 3, 4, 5 ] |> Array.fromList


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
                    [ Topology.vertices >> Expect.equal []
                    , Topology.edges >> Expect.equal []
                    , Topology.faces >> Expect.equal []
                    ]
        )


goodFaceList : Test
goodFaceList =
    Test.test "goodFaceList"
        (\() ->
            Topology.fromOrientedFaces octahedronVertices octahedronFaces
                |> Result.withDefault Topology.empty
                |> Expect.all
                    [ Topology.vertices
                        >> Expect.equal (List.range 0 5)
                    , Topology.edges
                        >> List.sort
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
                    , Topology.faces
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
                    , \mesh ->
                        Topology.vertices mesh
                            |> List.map (\v -> ( v, Topology.neighbors v mesh ))
                            |> Expect.equal
                                [ ( 0, [ 1, 2, 4, 5 ] )
                                , ( 1, [ 0, 5, 3, 2 ] )
                                , ( 2, [ 0, 1, 3, 4 ] )
                                , ( 3, [ 1, 5, 4, 2 ] )
                                , ( 4, [ 0, 2, 3, 5 ] )
                                , ( 5, [ 0, 4, 3, 1 ] )
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
            Topology.fromOrientedFaces
                (Array.fromList [ 'a', 'b', 'c', 'd' ])
                [ [ 0, 1, 2, 3 ], [ 3, 2, 1, 0 ] ]
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
                        >> Expect.equal [ 'a', 'b', 'c', 'd', 'e', 'f' ]
                    , Topology.faces
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
                    ]
        )