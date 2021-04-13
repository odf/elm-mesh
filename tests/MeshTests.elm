module MeshTests exposing
    ( combine
    , empty
    , fromTriangular
    , goodFaceList
    , mapVertices
    , orientationMismatch
    , subdivide
    , subdivideSmoothly
    , toTriangular
    , unpairedOrientedEdge
    , withNormals
    )

import Array exposing (Array)
import Expect
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Mesh
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


octahedron : Mesh.Mesh Vec3
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
    Mesh.fromOrientedFaces vertices faceIndices
        |> Result.withDefault Mesh.empty


empty : Test
empty =
    Test.test "empty"
        (\() ->
            Mesh.empty
                |> Expect.all
                    [ Mesh.vertices >> Array.toList >> Expect.equal []
                    , Mesh.edgeIndices >> Expect.equal []
                    , Mesh.faceIndices >> Expect.equal []
                    ]
        )


goodFaceList : Test
goodFaceList =
    Test.test "goodFaceList"
        (\() ->
            octahedronFaces
                |> Mesh.fromOrientedFaces octahedronVertices
                |> Result.withDefault Mesh.empty
                |> Expect.all
                    [ Mesh.vertices
                        >> Expect.equal octahedronVertices
                    , Mesh.edgeIndices
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
                    , Mesh.edgeVertices
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
                    , Mesh.faceIndices
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
                    , Mesh.faceVertices
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
                    , Mesh.neighborIndices
                        >> Array.toList
                        >> Expect.equal
                            [ [ 1, 2, 4, 5 ]
                            , [ 0, 5, 3, 2 ]
                            , [ 0, 1, 3, 4 ]
                            , [ 1, 5, 4, 2 ]
                            , [ 0, 2, 3, 5 ]
                            , [ 0, 4, 3, 1 ]
                            ]
                    , Mesh.neighborVertices
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
                |> Mesh.fromOrientedFaces octahedronVertices
                |> Expect.err
        )


orientationMismatch : Test
orientationMismatch =
    Test.test "orientationMismatch"
        (\() ->
            octahedronFaces
                |> List.drop 1
                |> (::) [ 0, 2, 1 ]
                |> Mesh.fromOrientedFaces octahedronVertices
                |> Expect.err
        )


toTriangular : Test
toTriangular =
    Test.test "toTriangular"
        (\() ->
            [ [ 0, 1, 2, 3 ], [ 3, 2, 1, 0 ] ]
                |> Mesh.fromOrientedFaces
                    (Array.fromList [ 'a', 'b', 'c', 'd' ])
                |> Result.withDefault Mesh.empty
                |> Mesh.toTriangularMesh
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
                |> Mesh.fromTriangularMesh
                |> Result.withDefault Mesh.empty
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal [ 'a', 'b', 'c', 'd', 'e', 'f' ]
                    , Mesh.faceIndices
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
                    , Mesh.faceVertices
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
            Mesh.fromOrientedFaces octahedronVertices octahedronFaces
                |> Result.withDefault Mesh.empty
                |> Mesh.mapVertices (String.slice 0 2 >> String.toUpper)
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal [ "FR", "RI", "TO", "BA", "LE", "BO" ]
                    ]
        )


combine : Test
combine =
    let
        tetra =
            Mesh.fromOrientedFaces
                (Array.fromList [ 0, 1, 2, 3 ])
                [ [ 0, 1, 2 ], [ 0, 2, 3 ], [ 0, 3, 1 ], [ 3, 2, 1 ] ]
                |> Result.withDefault Mesh.empty
    in
    Test.test "combine"
        (\() ->
            Mesh.combine [ tetra, Mesh.mapVertices ((+) 4) tetra ]
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal [ 0, 1, 2, 3, 4, 5, 6, 7 ]
                    , Mesh.faceIndices
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
                |> Mesh.withNormals identity Tuple.pair
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.map Tuple.second
                        >> Expect.equal (Mesh.vertices octahedron)
                    , Mesh.faceIndices
                        >> Expect.equal (Mesh.faceIndices octahedron)
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
                |> Mesh.mapVertices (Vec3.scale 6)
                |> Mesh.subdivide centroid
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.length
                        >> Expect.equal 26
                    , Mesh.edgeIndices
                        >> List.length
                        >> Expect.equal 48
                    , Mesh.faceIndices
                        >> List.length
                        >> Expect.equal 24
                    , Mesh.faceIndices
                        >> List.map List.length
                        >> Expect.equal (List.repeat 24 4)
                    , Mesh.faceIndices
                        >> List.map (List.filter (\i -> i < 6) >> List.length)
                        >> Expect.equal (List.repeat 24 1)
                    , Mesh.faceIndices
                        >> List.map (List.filter (\i -> i < 18) >> List.length)
                        >> Expect.equal (List.repeat 24 3)
                    , Mesh.neighborIndices
                        >> Array.toList
                        >> List.map List.length
                        >> Expect.equal (List.repeat 18 4 ++ List.repeat 8 3)
                    , Mesh.vertices
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


subdivideSmoothly : Test
subdivideSmoothly =
    Test.test "subdivideSmoothly"
        (\() ->
            let
                baseMesh =
                    octahedron

                simpleSubdivision =
                    Mesh.subdivide centroid baseMesh
            in
            baseMesh
                |> Mesh.mapVertices (Vec3.scale 12)
                |> Mesh.subdivideSmoothly
                    (always False)
                    identity
                    (\_ position -> position)
                |> Expect.all
                    [ Mesh.faceIndices
                        >> Expect.equal
                            (Mesh.faceIndices simpleSubdivision)
                    , Mesh.vertices
                        >> Array.toList
                        >> List.map
                            (\p -> ( Vec3.getX p, Vec3.getY p, Vec3.getZ p ))
                        >> List.sort
                        >> Expect.equalLists
                            [ ( -7, 0, 0 )
                            , ( -5, -5, 0 )
                            , ( -5, 0, -5 )
                            , ( -5, 0, 5 )
                            , ( -5, 5, 0 )
                            , ( -4, -4, -4 )
                            , ( -4, -4, 4 )
                            , ( -4, 4, -4 )
                            , ( -4, 4, 4 )
                            , ( 0, -7, 0 )
                            , ( 0, -5, -5 )
                            , ( 0, -5, 5 )
                            , ( 0, 0, -7 )
                            , ( 0, 0, 7 )
                            , ( 0, 5, -5 )
                            , ( 0, 5, 5 )
                            , ( 0, 7, 0 )
                            , ( 4, -4, -4 )
                            , ( 4, -4, 4 )
                            , ( 4, 4, -4 )
                            , ( 4, 4, 4 )
                            , ( 5, -5, 0 )
                            , ( 5, 0, -5 )
                            , ( 5, 0, 5 )
                            , ( 5, 5, 0 )
                            , ( 7, 0, 0 )
                            ]
                    ]
        )
