module MeshTests exposing
    ( ball
    , combine
    , empty
    , emptyFace
    , fromTriangular
    , fromTriangularWithBoundary
    , goodFaceList
    , grid
    , indexedBall
    , indexedBallEmpty
    , indexedBallMinimal
    , indexedGrid
    , indexedGridEmpty
    , indexedGridMinimal
    , indexedGridNegativeSteps
    , indexedRing
    , indexedRingEmpty
    , indexedTube
    , indexedTubeEmpty
    , indexedTubeMinimal
    , mapVertices
    , oneGon
    , orientationMismatch
    , ring
    , singleTwoGon
    , subdivide
    , subdivideSmoothly
    , subdivideSmoothlyWithBoundary
    , subdivideWithBoundary
    , toTriangular
    , toTriangularWithBoundary
    , tube
    , undefinedVertex
    , unreferencedVertex
    , vertexDuplicateInBoundary
    , vertexDuplicateInFace
    , withBoundary
    , withNormals
    , withNormalsWithBoundary
    )

import Array exposing (Array)
import Expect
import Length
import Mesh
import Point3d exposing (Point3d)
import Test exposing (Test)
import TriangularMesh
import Vector3d


type WorldCoordinates
    = WorldCoordinates


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


octahedron : Mesh.Mesh (Point3d Length.Meters WorldCoordinates)
octahedron =
    let
        vertices =
            Array.fromList
                [ Point3d.meters 1 0 0
                , Point3d.meters 0 1 0
                , Point3d.meters 0 0 1
                , Point3d.meters -1 0 0
                , Point3d.meters 0 -1 0
                , Point3d.meters 0 0 -1
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
                    , Mesh.boundaryIndices
                        >> Expect.equal []
                    , Mesh.boundaryVertices
                        >> Expect.equal []
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


withBoundary : Test
withBoundary =
    Test.test "withBoundary"
        (\() ->
            [ [ 1, 0, 5 ]
            , [ 2, 1, 3 ]
            , [ 0, 2, 4 ]
            , [ 5, 3, 1 ]
            , [ 4, 5, 0 ]
            , [ 3, 4, 2 ]
            ]
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
                            [ [ 0, 2, 4 ]
                            , [ 0, 4, 5 ]
                            , [ 0, 5, 1 ]
                            , [ 1, 3, 2 ]
                            , [ 1, 5, 3 ]
                            , [ 2, 3, 4 ]
                            ]
                    , Mesh.faceVertices
                        >> List.sort
                        >> Expect.equal
                            [ [ "front", "bottom", "right" ]
                            , [ "front", "left", "bottom" ]
                            , [ "front", "top", "left" ]
                            , [ "right", "back", "top" ]
                            , [ "right", "bottom", "back" ]
                            , [ "top", "back", "left" ]
                            ]
                    , Mesh.boundaryIndices
                        >> List.sort
                        >> Expect.equal
                            [ [ 0, 1, 2 ], [ 3, 5, 4 ] ]
                    , Mesh.boundaryVertices
                        >> List.sort
                        >> Expect.equal
                            [ [ "back", "bottom", "left" ]
                            , [ "front", "right", "top" ]
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


undefinedVertex : Test
undefinedVertex =
    Test.test "undefinedVertex"
        (\() ->
            Mesh.fromOrientedFaces
                (Array.slice 1 6 octahedronVertices)
                octahedronFaces
                |> Expect.err
        )


unreferencedVertex : Test
unreferencedVertex =
    Test.test "unreferencedVertex"
        (\() ->
            Mesh.fromOrientedFaces
                (Array.push "off" octahedronVertices)
                octahedronFaces
                |> Expect.err
        )


vertexDuplicateInFace : Test
vertexDuplicateInFace =
    Test.test "vertexDuplicateInFace"
        (\() ->
            [ [ 0, 1, 2, 0, 4, 5 ]
            , [ 1, 0, 5 ]
            , [ 2, 1, 3 ]
            , [ 0, 2, 4 ]
            , [ 3, 5, 4 ]
            , [ 5, 3, 1 ]
            , [ 3, 4, 2 ]
            ]
                |> Mesh.fromOrientedFaces octahedronVertices
                |> Expect.err
        )


vertexDuplicateInBoundary : Test
vertexDuplicateInBoundary =
    Test.test "vertexDuplicateInBoundary"
        (\() ->
            [ [ 1, 0, 5 ]
            , [ 2, 1, 3 ]
            , [ 0, 2, 4 ]
            , [ 3, 5, 4 ]
            , [ 5, 3, 1 ]
            , [ 3, 4, 2 ]
            ]
                |> Mesh.fromOrientedFaces octahedronVertices
                |> Expect.err
        )


emptyFace : Test
emptyFace =
    Test.test "emptyFace"
        (\() ->
            octahedronFaces
                |> (::) []
                |> Mesh.fromOrientedFaces octahedronVertices
                |> Expect.err
        )


oneGon : Test
oneGon =
    Test.test "oneGon"
        (\() ->
            octahedronFaces
                |> (::) [ 0 ]
                |> Mesh.fromOrientedFaces octahedronVertices
                |> Expect.err
        )


singleTwoGon : Test
singleTwoGon =
    Test.test "singleTwoGon"
        (\() ->
            Mesh.fromOrientedFaces (Array.fromList [ "a", "b" ]) [ [ 0, 1 ] ]
                |> Expect.ok
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


toTriangularWithBoundary : Test
toTriangularWithBoundary =
    Test.test "toTriangularWithBoundary"
        (\() ->
            [ [ 0, 1, 2, 3 ] ]
                |> Mesh.fromOrientedFaces
                    (Array.fromList [ 'a', 'b', 'c', 'd' ])
                |> Result.withDefault Mesh.empty
                |> Mesh.toTriangularMesh
                |> Expect.all
                    [ TriangularMesh.vertices
                        >> Array.toList
                        >> Expect.equal [ 'a', 'b', 'c', 'd' ]
                    , TriangularMesh.faceIndices
                        >> Expect.equal [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
                    , TriangularMesh.faceVertices
                        >> Expect.equal
                            [ ( 'a', 'b', 'c' ), ( 'a', 'c', 'd' ) ]
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


fromTriangularWithBoundary : Test
fromTriangularWithBoundary =
    Test.test "fromTriangularWithBoundary"
        (\() ->
            TriangularMesh.indexed
                (Array.fromList [ 'a', 'b', 'c', 'd', 'e', 'f' ])
                [ ( 1, 0, 5 )
                , ( 2, 1, 3 )
                , ( 0, 2, 4 )
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
                            [ [ 0, 2, 4 ]
                            , [ 0, 4, 5 ]
                            , [ 0, 5, 1 ]
                            , [ 1, 3, 2 ]
                            , [ 1, 5, 3 ]
                            , [ 2, 3, 4 ]
                            ]
                    , Mesh.faceVertices
                        >> List.sort
                        >> Expect.equal
                            [ [ 'a', 'c', 'e' ]
                            , [ 'a', 'e', 'f' ]
                            , [ 'a', 'f', 'b' ]
                            , [ 'b', 'd', 'c' ]
                            , [ 'b', 'f', 'd' ]
                            , [ 'c', 'd', 'e' ]
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
                        >> Array.map Vector3d.toUnitless
                        >> Array.map Vector3d.fromMeters
                        >> Array.map
                            (\v -> Point3d.translateBy v Point3d.origin)
                        >> Expect.equal (Mesh.vertices octahedron)
                    , Mesh.faceIndices
                        >> Expect.equal (Mesh.faceIndices octahedron)
                    ]
        )


withNormalsWithBoundary : Test
withNormalsWithBoundary =
    Test.test "withNormalsWithBoundary"
        (\() ->
            let
                verts =
                    [ Point3d.meters 1 0 -1
                    , Point3d.meters 0 1 -1
                    , Point3d.meters -1 0 -1
                    , Point3d.meters 0 -1 -1
                    , Point3d.meters 1 0 1
                    , Point3d.meters 0 1 1
                    , Point3d.meters -1 0 1
                    , Point3d.meters 0 -1 1
                    ]
                        |> Array.fromList

                faces =
                    [ [ 0, 1, 5, 4 ]
                    , [ 1, 2, 6, 5 ]
                    , [ 2, 3, 7, 6 ]
                    , [ 0, 4, 7, 3 ]
                    ]

                normals =
                    [ Vector3d.unitless 1 0 0
                    , Vector3d.unitless 0 1 0
                    , Vector3d.unitless -1 0 0
                    , Vector3d.unitless 0 -1 0
                    , Vector3d.unitless 1 0 0
                    , Vector3d.unitless 0 1 0
                    , Vector3d.unitless -1 0 0
                    , Vector3d.unitless 0 -1 0
                    ]
            in
            Mesh.fromOrientedFaces verts faces
                |> Result.withDefault Mesh.empty
                |> Mesh.withNormals identity Tuple.pair
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.map Tuple.first
                        >> Expect.equal verts
                    , Mesh.faceIndices
                        >> Expect.equal faces
                    , Mesh.vertices
                        >> Array.map Tuple.second
                        >> Array.toList
                        >> Expect.equal normals
                    ]
        )


centroid : List (Point3d units coordinates) -> Point3d units coordinates
centroid =
    Point3d.centroidN >> Maybe.withDefault Point3d.origin


subdivide : Test
subdivide =
    Test.test "subdivide"
        (\() ->
            octahedron
                |> Mesh.mapVertices (Point3d.scaleAbout Point3d.origin 6)
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
                        >> List.map (Point3d.toTuple Length.inMeters)
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


subdivideWithBoundary : Test
subdivideWithBoundary =
    Test.test "subdivideWithBoundary"
        (\() ->
            let
                makeVertex u v =
                    Point3d.meters (16 * u) (8 * v) 0

                baseMesh =
                    Mesh.grid 2 1 makeVertex

                simpleSubdivision =
                    Mesh.subdivide centroid baseMesh
            in
            simpleSubdivision
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> List.map (Point3d.toTuple Length.inMeters)
                        >> List.sort
                        >> Expect.equalLists
                            [ ( 0, 0, 0 )
                            , ( 0, 4, 0 )
                            , ( 0, 8, 0 )
                            , ( 4, 0, 0 )
                            , ( 4, 4, 0 )
                            , ( 4, 8, 0 )
                            , ( 8, 0, 0 )
                            , ( 8, 4, 0 )
                            , ( 8, 8, 0 )
                            , ( 12, 0, 0 )
                            , ( 12, 4, 0 )
                            , ( 12, 8, 0 )
                            , ( 16, 0, 0 )
                            , ( 16, 4, 0 )
                            , ( 16, 8, 0 )
                            ]
                    , Mesh.edgeIndices
                        >> List.length
                        >> Expect.equal 22
                    , Mesh.faceIndices
                        >> List.map List.length
                        >> Expect.equal (List.repeat 8 4)
                    , Mesh.faceIndices
                        >> List.map (List.filter (\i -> i < 6) >> List.length)
                        >> Expect.equal (List.repeat 8 1)
                    , Mesh.faceIndices
                        >> List.map (List.filter (\i -> i < 13) >> List.length)
                        >> Expect.equal (List.repeat 8 3)
                    , Mesh.neighborIndices
                        >> Array.toList
                        >> List.map List.length
                        >> List.sort
                        >> Expect.equal
                            (List.concat
                                [ List.repeat 4 2
                                , List.repeat 8 3
                                , List.repeat 3 4
                                ]
                            )
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
                |> Mesh.mapVertices (Point3d.scaleAbout Point3d.origin 12)
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
                        >> List.map (Point3d.toTuple Length.inMeters)
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


subdivideSmoothlyWithBoundary : Test
subdivideSmoothlyWithBoundary =
    Test.test "subdivideSmoothlyWithBoundary"
        (\() ->
            let
                makeVertex u v =
                    Point3d.meters (16 * u) (8 * v) 0

                baseMesh =
                    Mesh.grid 2 1 makeVertex

                simpleSubdivision =
                    Mesh.subdivide centroid baseMesh
            in
            baseMesh
                |> Mesh.subdivideSmoothly
                    (always False)
                    identity
                    (always identity)
                |> Expect.all
                    [ Mesh.faceIndices
                        >> Expect.equal
                            (Mesh.faceIndices simpleSubdivision)
                    , Mesh.vertices
                        >> Array.toList
                        >> List.map (Point3d.toTuple Length.inMeters)
                        >> List.sort
                        >> Expect.equalLists
                            [ ( 0, 4, 0 )
                            , ( 1, 1, 0 )
                            , ( 1, 7, 0 )
                            , ( 4, 0, 0 )
                            , ( 4, 4, 0 )
                            , ( 4, 8, 0 )
                            , ( 8, 0, 0 )
                            , ( 8, 4, 0 )
                            , ( 8, 8, 0 )
                            , ( 12, 0, 0 )
                            , ( 12, 4, 0 )
                            , ( 12, 8, 0 )
                            , ( 15, 1, 0 )
                            , ( 15, 7, 0 )
                            , ( 16, 4, 0 )
                            ]
                    ]
        )


indexedGrid : Test
indexedGrid =
    Test.test "indexedGrid"
        (\() ->
            Mesh.indexedGrid 3 2 Tuple.pair
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal
                            [ ( 0, 0 )
                            , ( 1, 0 )
                            , ( 2, 0 )
                            , ( 3, 0 )
                            , ( 0, 1 )
                            , ( 1, 1 )
                            , ( 2, 1 )
                            , ( 3, 1 )
                            , ( 0, 2 )
                            , ( 1, 2 )
                            , ( 2, 2 )
                            , ( 3, 2 )
                            ]
                    , Mesh.edgeIndices >> List.length >> Expect.equal 17
                    , Mesh.boundaryIndices >> List.length >> Expect.equal 1
                    , Mesh.faceIndices
                        >> List.map List.length
                        >> Expect.equal (List.repeat 6 4)
                    , Mesh.neighborIndices
                        >> Array.toList
                        >> List.map List.length
                        >> List.sort
                        >> Expect.equal [ 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4 ]
                    ]
        )


indexedGridMinimal : Test
indexedGridMinimal =
    Test.test "indexedGridMinimal"
        (\() ->
            Mesh.indexedGrid 1 1 Tuple.pair
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal
                            [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ]
                    , Mesh.edgeIndices >> List.length >> Expect.equal 4
                    , Mesh.boundaryIndices >> List.length >> Expect.equal 1
                    , Mesh.faceIndices
                        >> List.map List.length
                        >> Expect.equal [ 4 ]
                    , Mesh.neighborIndices
                        >> Array.toList
                        >> List.map List.length
                        >> Expect.equal [ 2, 2, 2, 2 ]
                    ]
        )


indexedGridEmpty : Test
indexedGridEmpty =
    Test.test "indexedGridEmpty"
        (\() ->
            Mesh.indexedGrid 3 0 Tuple.pair |> Expect.equal Mesh.empty
        )


indexedGridNegativeSteps : Test
indexedGridNegativeSteps =
    Test.test "indexedGridNegativeSteps"
        (\() ->
            Mesh.indexedGrid 3 -2 Tuple.pair
                |> Expect.equal Mesh.empty
        )


indexedTube : Test
indexedTube =
    Test.test "indexedTube"
        (\() ->
            Mesh.indexedTube 3 3 Tuple.pair
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal
                            [ ( 0, 0 )
                            , ( 1, 0 )
                            , ( 2, 0 )
                            , ( 3, 0 )
                            , ( 0, 1 )
                            , ( 1, 1 )
                            , ( 2, 1 )
                            , ( 3, 1 )
                            , ( 0, 2 )
                            , ( 1, 2 )
                            , ( 2, 2 )
                            , ( 3, 2 )
                            ]
                    , Mesh.edgeIndices >> List.length >> Expect.equal 21
                    , Mesh.boundaryIndices >> List.length >> Expect.equal 2
                    , Mesh.faceIndices
                        >> List.map List.length
                        >> Expect.equal (List.repeat 9 4)
                    , Mesh.neighborIndices
                        >> Array.toList
                        >> List.map List.length
                        >> List.sort
                        >> Expect.equal [ 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4 ]
                    ]
        )


indexedTubeMinimal : Test
indexedTubeMinimal =
    Test.test "indexedTubeMinimal"
        (\() ->
            Mesh.indexedTube 1 3 Tuple.pair
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal
                            [ ( 0, 0 )
                            , ( 1, 0 )
                            , ( 0, 1 )
                            , ( 1, 1 )
                            , ( 0, 2 )
                            , ( 1, 2 )
                            ]
                    , Mesh.edgeIndices >> List.length >> Expect.equal 9
                    , Mesh.boundaryIndices >> List.length >> Expect.equal 2
                    , Mesh.faceIndices
                        >> List.map List.length
                        >> Expect.equal [ 4, 4, 4 ]
                    , Mesh.neighborIndices
                        >> Array.toList
                        >> List.map List.length
                        >> Expect.equal (List.repeat 6 3)
                    ]
        )


indexedTubeEmpty : Test
indexedTubeEmpty =
    Test.test "indexedTubeEmpty"
        (\() ->
            Mesh.indexedTube 3 2 Tuple.pair |> Expect.equal Mesh.empty
        )


indexedRing : Test
indexedRing =
    Test.test "indexedRing"
        (\() ->
            Mesh.indexedRing 3 3 Tuple.pair
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal
                            [ ( 0, 0 )
                            , ( 1, 0 )
                            , ( 2, 0 )
                            , ( 0, 1 )
                            , ( 1, 1 )
                            , ( 2, 1 )
                            , ( 0, 2 )
                            , ( 1, 2 )
                            , ( 2, 2 )
                            ]
                    , Mesh.edgeIndices >> List.length >> Expect.equal 18
                    , Mesh.boundaryIndices >> List.length >> Expect.equal 0
                    , Mesh.faceIndices
                        >> List.map List.length
                        >> Expect.equal (List.repeat 9 4)
                    , Mesh.neighborIndices
                        >> Array.toList
                        >> List.map List.length
                        >> List.sort
                        >> Expect.equal (List.repeat 9 4)
                    ]
        )


indexedRingEmpty : Test
indexedRingEmpty =
    Test.test "indexedRingEmpty"
        (\() ->
            Mesh.indexedRing 2 3 Tuple.pair |> Expect.equal Mesh.empty
        )


indexedBall : Test
indexedBall =
    Test.test "indexedBall"
        (\() ->
            Mesh.indexedBall 3 3 Tuple.pair
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> List.sort
                        >> Expect.equal
                            [ ( 0, 0 )
                            , ( 0, 1 )
                            , ( 0, 2 )
                            , ( 0, 3 )
                            , ( 1, 1 )
                            , ( 1, 2 )
                            , ( 2, 1 )
                            , ( 2, 2 )
                            ]
                    , Mesh.edgeIndices >> List.length >> Expect.equal 15
                    , Mesh.boundaryIndices >> List.length >> Expect.equal 0
                    , Mesh.faceIndices
                        >> List.map List.length
                        >> List.sort
                        >> Expect.equal [ 3, 3, 3, 3, 3, 3, 4, 4, 4 ]
                    , Mesh.neighborIndices
                        >> Array.toList
                        >> List.map List.length
                        >> List.sort
                        >> Expect.equal [ 3, 3, 4, 4, 4, 4, 4, 4 ]
                    ]
        )


indexedBallMinimal : Test
indexedBallMinimal =
    Test.test "indexedBallMinimal"
        (\() ->
            Mesh.indexedBall 3 2 Tuple.pair
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> List.sort
                        >> Expect.equal
                            [ ( 0, 0 )
                            , ( 0, 1 )
                            , ( 0, 2 )
                            , ( 1, 1 )
                            , ( 2, 1 )
                            ]
                    , Mesh.edgeIndices >> List.length >> Expect.equal 9
                    , Mesh.boundaryIndices >> List.length >> Expect.equal 0
                    , Mesh.faceIndices
                        >> List.map List.length
                        >> List.sort
                        >> Expect.equal [ 3, 3, 3, 3, 3, 3 ]
                    , Mesh.neighborIndices
                        >> Array.toList
                        >> List.map List.length
                        >> List.sort
                        >> Expect.equal [ 3, 3, 4, 4, 4 ]
                    ]
        )


indexedBallEmpty : Test
indexedBallEmpty =
    Test.test "indexedBallEmpty"
        (\() ->
            Mesh.indexedBall 3 1 Tuple.pair |> Expect.equal Mesh.empty
        )


grid : Test
grid =
    Test.test "grid"
        (\() ->
            Mesh.grid 5 2 Tuple.pair
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal
                            [ ( 0.0, 0.0 )
                            , ( 0.2, 0.0 )
                            , ( 0.4, 0.0 )
                            , ( 0.6, 0.0 )
                            , ( 0.8, 0.0 )
                            , ( 1.0, 0.0 )
                            , ( 0.0, 0.5 )
                            , ( 0.2, 0.5 )
                            , ( 0.4, 0.5 )
                            , ( 0.6, 0.5 )
                            , ( 0.8, 0.5 )
                            , ( 1.0, 0.5 )
                            , ( 0.0, 1.0 )
                            , ( 0.2, 1.0 )
                            , ( 0.4, 1.0 )
                            , ( 0.6, 1.0 )
                            , ( 0.8, 1.0 )
                            , ( 1.0, 1.0 )
                            ]
                    , Mesh.edgeIndices >> List.length >> Expect.equal 27
                    , Mesh.boundaryIndices >> List.length >> Expect.equal 1
                    , Mesh.faceIndices
                        >> List.map List.length
                        >> Expect.equal (List.repeat 10 4)
                    , Mesh.neighborIndices
                        >> Array.toList
                        >> List.map List.length
                        >> List.sort
                        >> Expect.equal
                            (List.concat
                                [ List.repeat 4 2
                                , List.repeat 10 3
                                , List.repeat 4 4
                                ]
                            )
                    ]
        )


tube : Test
tube =
    Test.test "tube"
        (\() ->
            Mesh.tube 2 5 Tuple.pair
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal
                            [ ( 0.0, 0.0 )
                            , ( 0.5, 0.0 )
                            , ( 1.0, 0.0 )
                            , ( 0.0, 0.2 )
                            , ( 0.5, 0.2 )
                            , ( 1.0, 0.2 )
                            , ( 0.0, 0.4 )
                            , ( 0.5, 0.4 )
                            , ( 1.0, 0.4 )
                            , ( 0.0, 0.6 )
                            , ( 0.5, 0.6 )
                            , ( 1.0, 0.6 )
                            , ( 0.0, 0.8 )
                            , ( 0.5, 0.8 )
                            , ( 1.0, 0.8 )
                            ]
                    , Mesh.edgeIndices >> List.length >> Expect.equal 25
                    , Mesh.boundaryIndices >> List.length >> Expect.equal 2
                    , Mesh.faceIndices
                        >> List.map List.length
                        >> Expect.equal (List.repeat 10 4)
                    , Mesh.neighborIndices
                        >> Array.toList
                        >> List.map List.length
                        >> List.sort
                        >> Expect.equal
                            (List.concat [ List.repeat 10 3, List.repeat 5 4 ])
                    ]
        )


ring : Test
ring =
    Test.test "ring"
        (\() ->
            Mesh.ring 4 4 Tuple.pair
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> Expect.equal
                            [ ( 0.0, 0.0 )
                            , ( 0.25, 0.0 )
                            , ( 0.5, 0.0 )
                            , ( 0.75, 0.0 )
                            , ( 0.0, 0.25 )
                            , ( 0.25, 0.25 )
                            , ( 0.5, 0.25 )
                            , ( 0.75, 0.25 )
                            , ( 0.0, 0.5 )
                            , ( 0.25, 0.5 )
                            , ( 0.5, 0.5 )
                            , ( 0.75, 0.5 )
                            , ( 0.0, 0.75 )
                            , ( 0.25, 0.75 )
                            , ( 0.5, 0.75 )
                            , ( 0.75, 0.75 )
                            ]
                    , Mesh.edgeIndices >> List.length >> Expect.equal 32
                    , Mesh.boundaryIndices >> List.length >> Expect.equal 0
                    , Mesh.faceIndices
                        >> List.map List.length
                        >> Expect.equal (List.repeat 16 4)
                    , Mesh.neighborIndices
                        >> Array.toList
                        >> List.map List.length
                        >> List.sort
                        >> Expect.equal (List.repeat 16 4)
                    ]
        )


ball : Test
ball =
    Test.test "ball"
        (\() ->
            Mesh.ball 4 2 Tuple.pair
                |> Expect.all
                    [ Mesh.vertices
                        >> Array.toList
                        >> List.sort
                        >> Expect.equal
                            [ ( 0.0, 0.0 )
                            , ( 0.0, 0.5 )
                            , ( 0.0, 1.0 )
                            , ( 0.25, 0.5 )
                            , ( 0.5, 0.5 )
                            , ( 0.75, 0.5 )
                            ]
                    , Mesh.edgeIndices >> List.length >> Expect.equal 12
                    , Mesh.boundaryIndices >> List.length >> Expect.equal 0
                    , Mesh.faceIndices
                        >> List.map List.length
                        >> Expect.equal (List.repeat 8 3)
                    , Mesh.neighborIndices
                        >> Array.toList
                        >> List.map List.length
                        >> List.sort
                        >> Expect.equal (List.repeat 6 4)
                    ]
        )
