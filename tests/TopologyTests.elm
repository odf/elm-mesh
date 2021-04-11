module TopologyTests exposing
    ( combine
    , empty
    , fromTriangular
    , goodFaceList
    , mapVertices
    , orientationMismatch
    , subdivide
    , toTriangular
    , unpairedOrientedEdge
    , withNormals
    )

import Array exposing (Array)
import Expect
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
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


octahedron : Topology.Mesh Vec3
octahedron =
    let
        vertices =
            Array.fromList
                [ vec3 1 0 0
                , vec3 0 1 0
                , vec3 0 0 1
                , vec3 -1 0 0
                , vec3 0 -1 0
                , vec3 0 0 -1
                ]

        faceIndices =
            [ [ 0, 1, 2 ]
            , [ 1, 0, 5 ]
            , [ 2, 1, 3 ]
            , [ 0, 2, 4 ]
            , [ 3, 5, 4 ]
            , [ 5, 3, 1 ]
            , [ 4, 5, 0 ]
            , [ 3, 4, 2 ]
            ]
    in
    Topology.fromOrientedFaces vertices faceIndices
        |> Result.withDefault Topology.empty


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


withNormals : Test
withNormals =
    Test.test "withNormals"
        (\() ->
            octahedron
                |> Topology.withNormals identity Tuple.pair
                |> Expect.all
                    [ Topology.vertices
                        >> Array.map Tuple.second
                        >> Expect.equal (Topology.vertices octahedron)
                    , Topology.faceIndices
                        >> Expect.equal (Topology.faceIndices octahedron)
                    ]
        )


centroid : List Vec3 -> Vec3
centroid points =
    List.foldl Vec3.add (vec3 0 0 0) points
        |> Vec3.scale (1 / toFloat (List.length points))


subdivide : Test
subdivide =
    Test.test "subdivide"
        (\() ->
            octahedron
                |> Topology.mapVertices (Vec3.scale 6)
                |> Topology.subdivision centroid
                |> Expect.all
                    [ Topology.vertices
                        >> Array.length
                        >> Expect.equal 26
                    , Topology.edgeIndices
                        >> List.length
                        >> Expect.equal 48
                    , Topology.faceIndices
                        >> List.length
                        >> Expect.equal 24
                    , Topology.faceIndices
                        >> List.map List.length
                        >> Expect.equal (List.repeat 24 4)
                    , Topology.faceIndices
                        >> List.map (List.filter (\i -> i < 6) >> List.length)
                        >> Expect.equal (List.repeat 24 1)
                    , Topology.faceIndices
                        >> List.map (List.filter (\i -> i < 18) >> List.length)
                        >> Expect.equal (List.repeat 24 3)
                    , Topology.neighborIndices
                        >> Array.toList
                        >> List.map List.length
                        >> Expect.equal (List.repeat 18 4 ++ List.repeat 8 3)
                    , Topology.vertices
                        >> Array.toList
                        >> List.map
                            (\p -> ( Vec3.getX p, Vec3.getY p, Vec3.getZ p ))
                        >> List.sort
                        >> Expect.equalLists
                            [ ( -6, 0, 0 )
                            , ( -3, -3, 0 )
                            , ( -3, 0, -3 )
                            , ( -3, 0, 3 )
                            , ( -3, 3, 0 )
                            , ( -2, -2, -2 )
                            , ( -2, -2, 2 )
                            , ( -2, 2, -2 )
                            , ( -2, 2, 2 )
                            , ( 0, -6, 0 )
                            , ( 0, -3, -3 )
                            , ( 0, -3, 3 )
                            , ( 0, 0, -6 )
                            , ( 0, 0, 6 )
                            , ( 0, 3, -3 )
                            , ( 0, 3, 3 )
                            , ( 0, 6, 0 )
                            , ( 2, -2, -2 )
                            , ( 2, -2, 2 )
                            , ( 2, 2, -2 )
                            , ( 2, 2, 2 )
                            , ( 3, -3, 0 )
                            , ( 3, 0, -3 )
                            , ( 3, 0, 3 )
                            , ( 3, 3, 0 )
                            , ( 6, 0, 0 )
                            ]
                    ]
        )
