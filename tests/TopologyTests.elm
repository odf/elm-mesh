module TopologyTests exposing
    ( base
    , fromTriangular
    , goodFaceList
    , orientationMismatch
    , toTriangular
    , unpairedOrientedEdge
    )

import Array
import Expect
import Mesh.Topology as Topology
import Test exposing (Test)
import TriangularMesh


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


base : Test
base =
    Test.test "base"
        (\() ->
            Topology.base
                |> Expect.all
                    [ Topology.vertices >> Expect.equal [ 0, 1 ]
                    , Topology.edges >> Expect.equal [ ( 0, 1 ) ]
                    , Topology.faces >> Expect.equal [ [ 0, 1 ] ]
                    , Topology.neighbors 0 >> Expect.equal [ 1 ]
                    , Topology.neighbors 1 >> Expect.equal [ 0 ]
                    ]
        )


goodFaceList : Test
goodFaceList =
    Test.test "goodFaceList"
        (\() ->
            Topology.fromOrientedFaces octahedronFaces
                |> Result.withDefault Topology.base
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
                |> Topology.fromOrientedFaces
                |> Expect.err
        )


orientationMismatch : Test
orientationMismatch =
    Test.test "orientationMismatch"
        (\() ->
            octahedronFaces
                |> List.drop 1
                |> (::) [ 0, 2, 1 ]
                |> Topology.fromOrientedFaces
                |> Expect.err
        )


toTriangular : Test
toTriangular =
    Test.test "toTriangular"
        (\() ->
            Topology.fromOrientedFaces [ [ 0, 1, 2, 3 ], [ 3, 2, 1, 0 ] ]
                |> Result.withDefault Topology.base
                |> Topology.toTriangularMesh
                |> Expect.all
                    [ TriangularMesh.vertices
                        >> Array.toList
                        >> Expect.equal [ 0, 1, 2, 3 ]
                    , TriangularMesh.faceIndices
                        >> Expect.equal
                            [ ( 0, 1, 2 )
                            , ( 0, 2, 3 )
                            , ( 0, 3, 2 )
                            , ( 0, 2, 1 )
                            ]
                    , TriangularMesh.faceVertices
                        >> Expect.equal
                            [ ( 0, 1, 2 )
                            , ( 0, 2, 3 )
                            , ( 0, 3, 2 )
                            , ( 0, 2, 1 )
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
                |> Result.withDefault Topology.base
                |> Expect.all
                    [ Topology.vertices
                        >> Expect.equal [ 0, 1, 2, 3, 4, 5 ]
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
