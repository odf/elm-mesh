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
import Mesh.Topology as Topology
import Test exposing (Test)
import TriangularMesh


octahedronVertices : Array Int
octahedronVertices =
    Array.fromList [ 0, 1, 2, 3, 4, 5 ]


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
                        >> Array.toList
                        >> Expect.equal (List.range 0 5)
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
                    , \mesh ->
                        Topology.vertices mesh
                            |> Array.toList
                            |> List.map
                                (\v -> ( v, Topology.neighborIndices v mesh ))
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
                            [ [ 0, 1, 2 ] -- [ 'a', 'b', 'c' ]
                            , [ 0, 2, 4 ] -- [ 'a', 'c', 'e' ]
                            , [ 0, 4, 5 ] -- [ 'a', 'e', 'f' ]
                            , [ 0, 5, 1 ] -- [ 'a', 'f', 'b' ]
                            , [ 1, 3, 2 ] -- [ 'b', 'd', 'c' ]
                            , [ 1, 5, 3 ] -- [ 'b', 'f', 'd' ]
                            , [ 2, 3, 4 ] -- [ 'c', 'd', 'e' ]
                            , [ 3, 5, 4 ] -- [ 'd', 'f', 'e' ]
                            ]
                    ]
        )
