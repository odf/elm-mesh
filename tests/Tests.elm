module Tests exposing
    ( combine
    , edgeVertices
    , empty
    , faceVertices
    , faces
    , fromTriangular
    , joinVertices
    , mapVertices
    , neighbors
    , subD
    , subdivide
    , toTriangular
    , withNormals
    )

import Array
import Dict
import Expect
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Mesh exposing (Mesh)
import Test exposing (Test)
import TriangularMesh


square : Mesh Char
square =
    let
        vertices =
            Array.fromList [ 'a', 'b', 'c', 'd' ]

        faceIndices =
            [ [ 0, 1, 2, 3 ] ]
    in
    Mesh.indexed vertices faceIndices


triangle : Mesh Char
triangle =
    let
        vertices =
            Array.fromList [ 'e', 'f', 'g' ]

        faceIndices =
            [ [ 0, 1, 2 ] ]
    in
    Mesh.indexed vertices faceIndices


octahedron : Mesh Vec3
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
    Mesh.indexed vertices faceIndices


empty : Test
empty =
    Test.test "empty"
        (\() ->
            Mesh.empty
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal []
                    , Mesh.faceIndices
                        >> Expect.equal []
                    ]
        )


faces : Test
faces =
    Test.test "faces"
        (\() ->
            Mesh.faces
                [ [ 'a', 'b', 'c' ]
                , [ 'a', 'c', 'd' ]
                ]
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal [ 'a', 'b', 'c', 'a', 'c', 'd' ]
                    , Mesh.faceIndices
                        >> Expect.equal
                            [ [ 0, 1, 2 ]
                            , [ 3, 4, 5 ]
                            ]
                    , Mesh.faceVertices
                        >> Expect.equal
                            [ [ 'a', 'b', 'c' ]
                            , [ 'a', 'c', 'd' ]
                            ]
                    ]
        )


faceVertices : Test
faceVertices =
    Test.test "faceVertices"
        (\() ->
            Mesh.faceVertices square
                |> Expect.equal [ [ 'a', 'b', 'c', 'd' ] ]
        )


edgeVertices : Test
edgeVertices =
    Test.test "edgeVertices"
        (\() ->
            Mesh.edgeVertices square
                |> Expect.equal
                    [ ( 'a', 'b' )
                    , ( 'a', 'd' )
                    , ( 'b', 'c' )
                    , ( 'c', 'd' )
                    ]
        )


neighbors : Test
neighbors =
    Test.test "neighbors"
        (\() ->
            Mesh.neighbors square
                |> Dict.toList
                |> Expect.equal
                    [ ( 0, [ 1, 3 ] )
                    , ( 1, [ 0, 2 ] )
                    , ( 2, [ 1, 3 ] )
                    , ( 3, [ 0, 2 ] )
                    ]
        )


mapVertices : Test
mapVertices =
    Test.test "mapVertices"
        (\() ->
            Mesh.mapVertices ((-) 100 << Char.toCode) square
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal [ 3, 2, 1, 0 ]
                    , Mesh.faceVertices
                        >> Expect.equal [ [ 3, 2, 1, 0 ] ]
                    ]
        )


combine : Test
combine =
    let
        meshes =
            [ triangle, square, triangle ]
    in
    Test.test "combine"
        (\() ->
            Mesh.combine meshes
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal
                            (List.map (Mesh.vertices >> Array.toList) meshes
                                |> List.concat
                            )
                    , Mesh.faceIndices
                        >> Expect.equal
                            [ [ 0, 1, 2 ]
                            , [ 3, 4, 5, 6 ]
                            , [ 7, 8, 9 ]
                            ]
                    , Mesh.faceVertices
                        >> Expect.equal
                            (List.map (Mesh.vertices >> Array.toList) meshes)
                    ]
        )


joinVertices : Test
joinVertices =
    Test.test "joinVertices"
        (\() ->
            Mesh.faces
                [ [ 'a', 'b', 'c' ]
                , [ 'a', 'c', 'd' ]
                , [ 'd', 'c', 'b', 'a' ]
                ]
                |> Mesh.deduplicateVertices
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal [ 'a', 'b', 'c', 'd' ]
                    , Mesh.faceIndices
                        >> Expect.equal
                            [ [ 0, 1, 2 ]
                            , [ 0, 2, 3 ]
                            , [ 3, 2, 1, 0 ]
                            ]
                    , Mesh.faceVertices
                        >> Expect.equal
                            [ [ 'a', 'b', 'c' ]
                            , [ 'a', 'c', 'd' ]
                            , [ 'd', 'c', 'b', 'a' ]
                            ]
                    ]
        )


toTriangular : Test
toTriangular =
    Test.test "toTriangular"
        (\() ->
            Mesh.combine [ square, triangle ]
                |> Mesh.toTriangularMesh
                |> Expect.all
                    [ TriangularMesh.vertices
                        >> Array.toList
                        >> Expect.equal [ 'a', 'b', 'c', 'd', 'e', 'f', 'g' ]
                    , TriangularMesh.faceIndices
                        >> Expect.equal
                            [ ( 0, 1, 2 )
                            , ( 0, 2, 3 )
                            , ( 4, 5, 6 )
                            ]
                    , TriangularMesh.faceVertices
                        >> Expect.equal
                            [ ( 'a', 'b', 'c' )
                            , ( 'a', 'c', 'd' )
                            , ( 'e', 'f', 'g' )
                            ]
                    ]
        )


fromTriangular : Test
fromTriangular =
    Test.test "fromTriangular"
        (\() ->
            TriangularMesh.indexed
                (Array.fromList [ 'a', 'b', 'c', 'd' ])
                [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
                |> Mesh.fromTriangularMesh
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal [ 'a', 'b', 'c', 'd' ]
                    , Mesh.faceIndices
                        >> Expect.equal
                            [ [ 0, 1, 2 ]
                            , [ 0, 2, 3 ]
                            ]
                    , Mesh.faceVertices
                        >> Expect.equal
                            [ [ 'a', 'b', 'c' ]
                            , [ 'a', 'c', 'd' ]
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


subdivide : Test
subdivide =
    Test.test "subdivide"
        (\() ->
            octahedron
                |> Mesh.mapVertices (Vec3.scale 6)
                |> Mesh.subdivide identity (\_ position -> position)
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
                    , Mesh.neighbors
                        >> Dict.toList
                        >> List.map (Tuple.second >> List.length)
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


subD : Test
subD =
    Test.test "subD"
        (\() ->
            let
                baseMesh =
                    octahedron

                simpleSubdivision =
                    Mesh.subdivide identity (\_ position -> position) baseMesh
            in
            baseMesh
                |> Mesh.mapVertices (Vec3.scale 12)
                |> Mesh.subD (always False) identity (\_ position -> position)
                |> Expect.all
                    [ Mesh.faceIndices
                        >> Expect.equal (Mesh.faceIndices simpleSubdivision)
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
