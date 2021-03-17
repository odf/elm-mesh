module Tests exposing
    ( combine
    , edgeVertices
    , empty
    , faceVertices
    , faces
    , fromTriangular
    , joinVertices
    , mapVertices
    , toTriangular
    )

import Array
import Expect exposing (Expectation)
import Mesh exposing (Mesh)
import Test exposing (Test)
import TriangularMesh exposing (TriangularMesh)


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
    Test.test "combine"
        (\() ->
            Mesh.combine [ triangle, square, triangle ]
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal
                            (List.concat
                                [ [ 'e', 'f', 'g' ]
                                , [ 'a', 'b', 'c', 'd' ]
                                , [ 'e', 'f', 'g' ]
                                ]
                            )
                    , Mesh.faceIndices
                        >> Expect.equal
                            [ [ 0, 1, 2 ]
                            , [ 3, 4, 5, 6 ]
                            , [ 7, 8, 9 ]
                            ]
                    , Mesh.faceVertices
                        >> Expect.equal
                            [ [ 'e', 'f', 'g' ]
                            , [ 'a', 'b', 'c', 'd' ]
                            , [ 'e', 'f', 'g' ]
                            ]
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
                |> Mesh.joinVertices
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
