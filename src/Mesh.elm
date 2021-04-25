module Mesh exposing
    ( Mesh
    , empty
    , fromOrientedFaces
    , fromTriangularMesh, toTriangularMesh
    , combine
    , vertices, vertex, faceIndices, faceVertices
    , boundaryIndices, boundaryVertices
    , edgeIndices, edgeVertices, neighborIndices, neighborVertices
    , mapVertices, withNormals, subdivide, subdivideSmoothly
    , indexedBall, indexedGrid, indexedRing, indexedTube
    )

{-| This module provides functions for working with indexed meshes.
You can:

  - Construct meshes from vertices and face indices
  - Extract vertices, faces and edges in various ways
  - Combine multiple meshes into a single mesh

@docs Mesh


# Constants

@docs empty


# Constructors

@docs fromOrientedFaces


# Interop

@docs fromTriangularMesh, toTriangularMesh


# Combining meshes

@docs combine


# Properties

@docs vertices, vertex, faceIndices, faceVertices
@docs boundaryIndices, boundaryVertices
@docs edgeIndices, edgeVertices, neighborIndices, neighborVertices


# Transformations

@docs mapVertices, withNormals, subdivide, subdivideSmoothly

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import Set
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


{-| A `Mesh` is an instance of a
[polygon mesh](https://en.wikipedia.org/wiki/Polygon_mesh)
which is implemented as a so-called
[half-edge data structure](https://www.flipcode.com/archives/The_Half-Edge_Data_Structure.shtml).
Each mesh consists of an array of vertices and some connectivity information
that describes how faces, edges and vertices are related to each other. A face
must have at least three vertices, but can also have four, five or more.

The half-edge data structure guarantees that the mesh describes a surface with
a consistent "orientation", which intuitively means that if one were to walk
along on the surface of the mesh one could never find oneself back at the
starting point but standing upside-down on the other side of the mesh.

The vertices themselves can be any type you want. For a 2D mesh, you might have
each vertex be simply a point:

    type alias Mesh2d =
        Mesh Point2d

For a 3D mesh, each vertex might be a (point, normal) tuple:

    type alias Mesh3d =
        Mesh ( Point3d, Vector3d )

In more complex cases, each vertex might be a record:

    type alias VertexData =
        { position : Point3d
        , normal : Vector3d
        , color : Color
        }

    type alias RenderMesh =
        Mesh VertexData

-}
type Mesh vertex
    = Mesh
        (Array vertex)
        { atVertex : Array OrientedEdge
        , alongFace : Array OrientedEdge
        , alongBoundaryComponent : Array OrientedEdge
        , toFace : Dict OrientedEdge Int
        , toBoundaryComponent : Dict OrientedEdge Int
        , next : Dict OrientedEdge OrientedEdge
        }


type alias OrientedEdge =
    ( Int, Int )


opposite : OrientedEdge -> OrientedEdge
opposite ( from, to ) =
    ( to, from )


{-| A mesh with no vertices or faces.
-}
empty : Mesh vertex
empty =
    Mesh Array.empty
        { atVertex = Array.empty
        , alongFace = Array.empty
        , alongBoundaryComponent = Array.empty
        , toFace = Dict.empty
        , toBoundaryComponent = Dict.empty
        , next = Dict.empty
        }


fromOrientedFacesUnchecked : Array vertex -> List (List Int) -> Mesh vertex
fromOrientedFacesUnchecked vertexData faceLists =
    let
        orientedEdgeLists =
            List.map cyclicPairs faceLists

        orientedEdges =
            List.concat orientedEdgeLists

        orientedEdgeSet =
            Set.fromList orientedEdges

        boundaryEdges =
            List.filter
                (opposite >> flip Set.member orientedEdgeSet >> not)
                orientedEdges

        verticesAtBoundary =
            List.map Tuple.first boundaryEdges

        nextOnBoundary =
            Dict.fromList (List.map opposite boundaryEdges)

        boundaryLists =
            extractCycles verticesAtBoundary (flip Dict.get nextOnBoundary)
                |> List.map canonicalCircular
                |> List.map cyclicPairs

        atVertex =
            orientedEdges
                |> List.map (\( from, to ) -> ( from, ( from, to ) ))
                |> Dict.fromList
                |> Dict.values
                |> Array.fromList

        toFace =
            orientedEdgeLists
                |> List.indexedMap (\i -> List.map (\e -> ( e, i )))
                |> List.concat
                |> Dict.fromList

        alongFace =
            Dict.toList toFace
                |> List.map (\( key, val ) -> ( val, key ))
                |> Dict.fromList
                |> Dict.values
                |> Array.fromList

        toBoundaryComponent =
            boundaryLists
                |> List.indexedMap (\i -> List.map (\e -> ( e, i )))
                |> List.concat
                |> Dict.fromList

        alongBoundaryComponent =
            Dict.toList toBoundaryComponent
                |> List.map (\( key, val ) -> ( val, key ))
                |> Dict.fromList
                |> Dict.values
                |> Array.fromList

        next =
            List.concat
                [ List.concatMap cyclicPairs orientedEdgeLists
                , List.concatMap cyclicPairs boundaryLists
                ]
                |> Dict.fromList
    in
    Mesh vertexData
        { atVertex = atVertex
        , alongFace = alongFace
        , alongBoundaryComponent = alongBoundaryComponent
        , toFace = toFace
        , toBoundaryComponent = toBoundaryComponent
        , next = next
        }


{-| Create a mesh from an array of vertices and a list of face indices. For
example, to construct a square pyramid where `a` is the front right corner,
`b` is the front left corner, `c` is the back left corner, `d` is the back
right corner and `e` is the apex:

    vertices =
        Array.fromList [ a, b, c, d, e ]

    faceIndices =
        [ [ 0, 1, 2, 3 ]
        , [ 0, 4, 1 ]
        , [ 1, 4, 2 ]
        , [ 2, 4, 3 ]
        , [ 3, 4, 0 ]
        ]

    pyramid =
        Mesh.fromOrientedFaces vertices faceIndices
            |> Result.withDefault Mesh.empty

The return type here is a `Result String (Mesh vertex)` rather than just
a `Mesh vertex` so that a specific error message can be produced if there is
a problem with the input.

-}
fromOrientedFaces :
    Array vertex
    -> List (List Int)
    -> Result String (Mesh vertex)
fromOrientedFaces vertexData faceLists =
    let
        definedVertexSet =
            List.range 0 (Array.length vertexData - 1) |> Set.fromList

        referencedVertexSet =
            List.concat faceLists |> Set.fromList

        orientedEdges =
            List.map cyclicPairs faceLists |> List.concat

        orientedEdgeSet =
            Set.fromList orientedEdges

        boundaryEdges =
            List.filter
                (opposite >> flip Set.member orientedEdgeSet >> not)
                orientedEdges
    in
    if Set.size (Set.diff referencedVertexSet definedVertexSet) > 0 then
        Err "an undefined vertex appears in a face"

    else if Set.size (Set.diff definedVertexSet referencedVertexSet) > 0 then
        Err "one of the vertices does not appear in any face"

    else if List.any (List.length >> flip (<) 2) faceLists then
        Err "there is a face with fewer than two vertices"

    else if List.any hasDuplicates faceLists then
        Err "a vertex appears more than once in the same face"

    else if List.map Tuple.first boundaryEdges |> hasDuplicates then
        Err "a vertex appears more then once in a boundary"

    else if Set.size orientedEdgeSet < List.length orientedEdges then
        Err "an oriented edge appears more than once"

    else
        Ok (fromOrientedFacesUnchecked vertexData faceLists)


{-| Get the vertices of a mesh.

    Mesh.vertices pyramid
    --> Array.fromList [ a, b, c, d, e ]

-}
vertices : Mesh vertex -> Array vertex
vertices (Mesh verts _) =
    verts


{-| Get a particular vertex of a mesh by index. If the index is out of range,
returns `Nothing`.

    Mesh.vertex 1 pyramid
    --> Just b

    Mesh.vertex 5 pyramid
    --> Nothing

-}
vertex : Int -> Mesh vertex -> Maybe vertex
vertex index mesh =
    Array.get index (vertices mesh)


verticesInFace : OrientedEdge -> Mesh vertex -> List Int
verticesInFace start (Mesh _ mesh) =
    traceCycle start (flip Dict.get mesh.next)
        |> List.map Tuple.first


{-| Get the faces of a mesh as lists of vertex indices. Each face is
normalized so that its smallest vertex index comes first.

    Mesh.faceIndices pyramid
    --> [ [ 0, 1, 2, 3 ]
    --> , [ 0, 4, 1 ]
    --> , [ 1, 4, 2 ]
    --> , [ 2, 4, 3 ]
    --> , [ 0, 3, 4 ]
    --> ]

-}
faceIndices : Mesh vertex -> List (List Int)
faceIndices (Mesh verts mesh) =
    Array.toList mesh.alongFace
        |> List.map (\start -> verticesInFace start (Mesh verts mesh))
        |> List.map canonicalCircular


{-| Get the faces of a mesh as lists of vertices. The ordering of vertices
for each face is the same as in `faceIndices`.

    Mesh.faceVertices pyramid
    --> [ [ a, b, c, d ]
    --> , [ a, e, b ]
    --> , [ b, e, c ]
    --> , [ c, e, d ]
    --> , [ a, d, e ]
    --> ]

-}
faceVertices : Mesh vertex -> List (List vertex)
faceVertices mesh =
    let
        toFace =
            List.filterMap (\i -> vertex i mesh)
    in
    List.map toFace (faceIndices mesh)


{-| Get the boundary components of a mesh as lists of vertex indices. Each
component is normalized so that its smallest vertex index comes first.

    pyramidTop =
        Mesh.fromOrientedFaces
            (Array.fromList [ a, b, c, d, e ])
            [ [ 0, 4, 1 ]
            , [ 1, 4, 2 ]
            , [ 2, 4, 3 ]
            , [ 3, 4, 0 ] ]
            |> Result.withDefault Mesh.empty

    Mesh.faceIndices pyramidTop
    --> [ [ 0, 4, 1 ]
    --> , [ 1, 4, 2 ]
    --> , [ 2, 4, 3 ]
    --> , [ 0, 3, 4 ]
    --> ]

    Mesh.boundaryIndices pyramidTop
    --> [ [ 0, 1, 2, 3 ] ]

-}
boundaryIndices : Mesh vertex -> List (List Int)
boundaryIndices (Mesh verts mesh) =
    Array.toList mesh.alongBoundaryComponent
        |> List.map (\start -> verticesInFace start (Mesh verts mesh))
        |> List.map canonicalCircular


{-| Get the boundary components of a mesh as lists of vertices. The ordering of
vertices for each component is the same as in `faceIndices`.

    Mesh.boundaryVertices pyramidTop
    --> [ [ a, b, c, d ] ]

-}
boundaryVertices : Mesh vertex -> List (List vertex)
boundaryVertices mesh =
    let
        toFace =
            List.filterMap (\i -> vertex i mesh)
    in
    List.map toFace (boundaryIndices mesh)


{-| Get all of the edges of a mesh as pairs of vertex indices. Each edge will
only be returned once, with the lower-index vertex listed first, and will be
returned in sorted order.

    Mesh.edgeIndices pyramid
    --> [ ( 0, 1 )
    --> , ( 0, 3 )
    --> , ( 0, 4 )
    --> , ( 1, 2 )
    --> , ( 1, 4 )
    --> , ( 2, 3 )
    --> , ( 2, 4 )
    --> , ( 3, 4 )
    --> ]

-}
edgeIndices : Mesh vertex -> List ( Int, Int )
edgeIndices (Mesh _ mesh) =
    Dict.keys mesh.next
        |> List.map (\( from, to ) -> ( min from to, max from to ))
        |> Set.fromList
        |> Set.toList


{-| Get all of the edges of a mesh as pairs of vertices. Each edge will
only be returned once, with the lower-index vertex listed first, and will be
returned in sorted order (by index).

    Mesh.edgeVertices pyramid
    --> [ ( a, b )
    --> , ( a, d )
    --> , ( a, e )
    --> , ( b, c )
    --> , ( b, e )
    --> , ( c, d )
    --> , ( c, e )
    --> , ( d, e )
    --> ]

-}
edgeVertices : Mesh vertex -> List ( vertex, vertex )
edgeVertices mesh =
    let
        toEdge ( i, j ) =
            Maybe.map2 Tuple.pair (vertex i mesh) (vertex j mesh)
    in
    List.filterMap toEdge (edgeIndices mesh)


vertexNeighbors : OrientedEdge -> Mesh vertex -> List Int
vertexNeighbors start (Mesh _ mesh) =
    traceCycle start (opposite >> flip Dict.get mesh.next)
        |> List.map Tuple.second
        |> List.reverse


{-| Get the neighbors of all vertices as lists of vertex indices. The i-th
entry in the output array corresponds to the i-th vertex of the
mesh. The neighbors are ordered as one would encounter them while walking
around the vertex in the same direction (clockwise or counterclockwise) as
the vertices for each face are listed, starting from the one with
the lowest vertex index.

    Mesh.neighborIndices pyramid
    --> [ [ 1, 3, 4 ]
    --> , [ 0, 4, 2 ]
    --> , [ 1, 4, 3 ]
    --> , [ 0, 2, 4 ]
    --> , [ 0, 3, 2, 1 ]
    --> ]

-}
neighborIndices : Mesh vertex -> Array (List Int)
neighborIndices (Mesh verts mesh) =
    Array.toList mesh.atVertex
        |> List.map (\start -> vertexNeighbors start (Mesh verts mesh))
        |> List.map canonicalCircular
        |> Array.fromList


{-| Get the neighbors of all vertices as lists of vertices. Further details
are as in `neighborIndices`.
-}
neighborVertices : Mesh vertex -> Array (List vertex)
neighborVertices mesh =
    neighborIndices mesh |> Array.map (List.filterMap (\i -> vertex i mesh))


{-| Triangulate each face of the mesh and return the result as a
`TriangularMesh` instance.
-}
toTriangularMesh : Mesh vertex -> TriangularMesh vertex
toTriangularMesh mesh =
    TriangularMesh.indexed
        (vertices mesh)
        (faceIndices mesh |> List.concatMap triangulate)


{-| Try to convert a `TriangularMesh` instance into a `Mesh`. This could
potentially fail because a `Mesh` is more restrictive.
-}
fromTriangularMesh : TriangularMesh vertex -> Result String (Mesh vertex)
fromTriangularMesh trimesh =
    TriangularMesh.faceIndices trimesh
        |> List.map (\( u, v, w ) -> [ u, v, w ])
        |> fromOrientedFaces (TriangularMesh.vertices trimesh)


{-| Transform a mesh by applying the given function to each of its vertices. For
example, if you had a 2D mesh where each vertex was an `( x, y )` tuple and you
wanted to convert it to a 3D mesh on the XY plane, you might use

    mesh2d : Mesh ( Float, Float )
    mesh2d =
        ...

    to3d : ( Float, Float ) -> ( Float, Float, Float )
    to3d ( x, y ) =
        ( x, y, 0 )

    mesh3d : Mesh ( Float, Float, Float )
    mesh3d =
        Mesh.mapVertices to3d mesh2d

-}
mapVertices : (a -> b) -> Mesh a -> Mesh b
mapVertices fn (Mesh verts mesh) =
    Mesh (Array.map fn verts) mesh


{-| Combine a list of meshes into a single mesh. This concatenates the vertex
arrays of each mesh and adjusts face indices to refer to the combined vertex
array.

    pyramid =
        Mesh.fromOrientedFaces
            (Array.fromList [ a, b, c, d, e ])
            [ [ 0, 1, 2, 3 ]
            , [ 0, 4, 1 ]
            , [ 1, 4, 2 ]
            , [ 2, 4, 3 ]
            , [ 3, 4, 0 ]
            ]

    tetrahedron =
        Mesh.fromOrientedFaces
            (Array.fromList [ A, B, C, D ])
            [ [ 0, 1, 2 ]
            , [ 0, 3, 1 ]
            , [ 1, 3, 2 ]
            , [ 2, 3, 0 ]
            ]

    combined =
        Mesh.combine [ pyramid, tetrahedron ]

    Mesh.vertices combined
    --> Array.fromList [ a, b, c, d, e, A, B, C, D ]

    Mesh.faceIndices combined
    --> [ [ 0, 1, 2, 3 ]
    --> , [ 0, 4, 1 ]
    --> , [ 1, 4, 2 ]
    --> , [ 2, 4, 3 ]
    --> , [ 0, 3, 4 ]
    --> , [ 5, 6, 7 ]
    --> , [ 5, 8, 6 ]
    --> , [ 6, 8, 7 ]
    --> , [ 5, 7, 8 ]
    --> ]

-}
combine : List (Mesh a) -> Mesh a
combine meshes =
    let
        vertices_ =
            List.map (vertices >> Array.toList) meshes
                |> List.concat
                |> Array.fromList

        makeFaces offset meshes_ =
            case meshes_ of
                mesh :: rest ->
                    let
                        nextOffset =
                            offset + Array.length (vertices mesh)
                    in
                    List.map (List.map ((+) offset)) (faceIndices mesh)
                        ++ makeFaces nextOffset rest

                _ ->
                    []
    in
    fromOrientedFacesUnchecked vertices_ (makeFaces 0 meshes)


{-| Calculate normals for a mesh. The first argument is a function that takes
a vertex of the input mesh and returns its position as a
[Point3d](http://package.elm-lang.org/packages/ianmackenzie/elm-geometry/3.9.0/Point3d).
The second argument is a function that takes a vertex of the input mesh and
its computed normal as a
[Vector3d](http://package.elm-lang.org/packages/ianmackenzie/elm-geometry/3.9.0/Vector3d),
and returns the corresponding vertex for the output mesh.
-}
withNormals :
    (a -> Point3d units coordinates)
    -> (a -> Vector3d Unitless coordinates -> b)
    -> Mesh a
    -> Mesh b
withNormals toPositionIn toVertexOut (Mesh verts mesh) =
    let
        neighbors =
            neighborIndices (Mesh verts mesh)

        positions =
            Array.map toPositionIn verts

        sectorArea a b c =
            case List.filterMap (flip Array.get positions) [ a, b, c ] of
                p :: q :: r :: [] ->
                    Vector3d.from q p |> Vector3d.cross (Vector3d.from q r)

                _ ->
                    Vector3d.zero

        isAtFace a b =
            Dict.get ( a, b ) mesh.toFace /= Nothing

        mapVertex idx v =
            Array.get idx neighbors
                |> Maybe.withDefault []
                |> cyclicPairs
                |> List.filter (\( a, _ ) -> isAtFace idx a)
                |> List.map (\( a, b ) -> sectorArea a idx b)
                |> Vector3d.sum
                |> Vector3d.normalize
                |> toVertexOut v
    in
    Mesh (Array.indexedMap mapVertex verts) mesh


{-| Subdivide a mesh by first adding a new vertex in the center of each edge,
subdividing each edge into two in the process, then adding a new vertex in the
center of each face and splitting the face into quadrangles by connecting the
new face center to the new edge centers.

The first argument is a function that takes a list of input vertices, namely
the vertices of an edge or face to be subdivided, and returns the new "center"
vertex.

-}
subdivide : (List vertex -> vertex) -> Mesh vertex -> Mesh vertex
subdivide composeFn (Mesh verts mesh) =
    let
        allEdges =
            edgeIndices (Mesh verts mesh)

        nrVertices =
            Array.length mesh.atVertex

        nrEdges =
            List.length allEdges

        midPointIndex =
            List.indexedMap (\i e -> ( e, i + nrVertices )) allEdges
                |> List.concatMap
                    (\( ( u, v ), i ) -> [ ( ( u, v ), i ), ( ( v, u ), i ) ])
                |> Dict.fromList

        vertsOut =
            ((List.range 0 (nrVertices - 1) |> List.map List.singleton)
                ++ (allEdges |> List.map (\( u, v ) -> [ u, v ]))
                ++ faceIndices (Mesh verts mesh)
            )
                |> List.map
                    (List.filterMap (\i -> Array.get i verts) >> composeFn)
                |> Array.fromList

        subFace ( u, v ) =
            let
                ( _, w ) =
                    Dict.get ( u, v ) mesh.next |> Maybe.withDefault ( v, u )
            in
            [ Just v
            , Dict.get ( v, w ) midPointIndex
            , Dict.get ( u, v ) mesh.toFace
                |> Maybe.map ((+) (nrVertices + nrEdges))
            , Dict.get ( u, v ) midPointIndex
            ]
                |> List.filterMap identity

        facesOut =
            Dict.keys mesh.next
                |> List.filter (\e -> Dict.get e mesh.toFace /= Nothing)
                |> List.map subFace
    in
    fromOrientedFacesUnchecked vertsOut facesOut


{-| Subdivide a mesh into quadrangles using the
[Catmull-Clark](https://en.wikipedia.org/wiki/Catmull%E2%80%93Clark_subdivision_surface)
method. The connectivity of the result will be the same as in `subdivide`, but
the vertices will be repositioned to make it appear smoother.

The first argument is a function that returns `True` if the input vertex should
not be moved. The second argument is a function that returns the position of
an input vertex. The third argument is a function that takes a list of input
vertices and a computed output position and returns an output vertex.

-}
subdivideSmoothly :
    (vertex -> Bool)
    -> (vertex -> Point3d units coordinates)
    -> (List vertex -> Point3d units coordinates -> vertex)
    -> Mesh vertex
    -> Mesh vertex
subdivideSmoothly isFixed vertexPosition toOutputVertex meshIn =
    let
        centroid =
            Point3d.centroidN >> Maybe.withDefault Point3d.origin

        verticesIn =
            vertices meshIn

        nrVertices =
            Array.length verticesIn

        nrEdges =
            List.length (edgeIndices meshIn)

        makeOutputVertex indices pos =
            toOutputVertex (getAll indices verticesIn) pos

        meshSub =
            meshIn
                |> mapVertices vertexPosition
                |> subdivide centroid

        boundaryIndicesSub =
            boundaryIndices meshSub
                |> List.concat
                |> Set.fromList

        subFaceIndices =
            faceIndices meshSub

        makeFacePoint k vs =
            vertex (k + nrVertices + nrEdges) meshSub
                |> Maybe.map (makeOutputVertex vs)

        facePoints =
            subFaceIndices
                |> List.indexedMap makeFacePoint
                |> List.filterMap identity

        neighborsSub =
            neighborIndices meshSub

        edgePointPosition i =
            if Set.member i boundaryIndicesSub then
                vertex (i + nrVertices) meshSub
                    |> Maybe.withDefault Point3d.origin

            else
                Array.get (i + nrVertices) neighborsSub
                    |> Maybe.withDefault [ i + nrVertices ]
                    |> List.filterMap (\k -> vertex k meshSub)
                    |> centroid

        makeEdgePoint i ( u, v ) =
            edgePointPosition i
                |> makeOutputVertex [ u, v ]

        edgePointPos =
            edgeIndices meshIn
                |> List.indexedMap (\i _ -> edgePointPosition i)
                |> Array.fromList

        edgePoints =
            edgeIndices meshIn
                |> List.indexedMap makeEdgePoint

        makeVertexPoint i =
            let
                posIn =
                    vertex i meshSub
                        |> Maybe.withDefault Point3d.origin
                        |> Vector3d.from Point3d.origin

                neighbors =
                    Array.get i neighborsSub |> Maybe.withDefault []

                nrNeighbors =
                    List.length neighbors

                centroidOfEdgeCenters =
                    neighbors
                        |> List.filterMap (\k -> vertex k meshSub)
                        |> centroid
                        |> Vector3d.from Point3d.origin

                centroidOfEdgePoints =
                    neighbors
                        |> List.map (\k -> k - nrVertices)
                        |> List.filterMap (\k -> Array.get k edgePointPos)
                        |> centroid
                        |> Vector3d.from Point3d.origin
            in
            if
                Array.get i verticesIn
                    |> Maybe.map isFixed
                    |> Maybe.withDefault False
            then
                Point3d.translateBy posIn Point3d.origin
                    |> makeOutputVertex [ i ]

            else if Set.member i boundaryIndicesSub then
                Vector3d.plus posIn centroidOfEdgeCenters
                    |> Vector3d.scaleBy 0.5
                    |> (\x -> Point3d.translateBy x Point3d.origin)
                    |> makeOutputVertex [ i ]

            else
                Vector3d.scaleBy (toFloat nrNeighbors - 3) posIn
                    |> Vector3d.plus centroidOfEdgeCenters
                    |> Vector3d.plus (Vector3d.scaleBy 2 centroidOfEdgePoints)
                    |> Vector3d.scaleBy (1 / toFloat nrNeighbors)
                    |> (\x -> Point3d.translateBy x Point3d.origin)
                    |> makeOutputVertex [ i ]

        vertexPoints =
            List.range 0 (nrVertices - 1) |> List.map makeVertexPoint

        verticesOut =
            Array.fromList (vertexPoints ++ edgePoints ++ facePoints)
    in
    fromOrientedFacesUnchecked verticesOut subFaceIndices


indexedGridData :
    Int
    -> Int
    -> Bool
    -> Bool
    -> ( Array ( Int, Int ), List (List Int) )
indexedGridData uSteps vSteps uClose vClose =
    if
        (uSteps * vSteps < 1)
            || (uClose && uSteps < 3)
            || (vClose && vSteps < 3)
    then
        ( Array.empty, [] )

    else
        let
            uCount =
                when uClose uSteps (uSteps + 1)

            vCount =
                when vClose vSteps (vSteps + 1)

            gridPoints =
                List.range 0 (uCount * vCount - 1)
                    |> List.map (\i -> ( i |> modBy uCount, i // uCount ))

            gridToIdx =
                gridPoints
                    |> List.indexedMap (\i p -> ( p, i ))
                    |> Dict.fromList

            faces =
                gridPoints
                    |> List.filter (\( u, v ) -> u < uSteps && v < vSteps)
                    |> List.map
                        (\( u, v ) ->
                            [ ( u, v )
                            , ( u |> incrMod uCount, v )
                            , ( u |> incrMod uCount, v |> incrMod vCount )
                            , ( u, v |> incrMod vCount )
                            ]
                                |> List.filterMap (flip Dict.get gridToIdx)
                        )
        in
        ( Array.fromList gridPoints, faces )


indexedGrid : Int -> Int -> (Int -> Int -> vertex) -> Mesh vertex
indexedGrid uSteps vSteps toVertex =
    let
        ( verts, faces ) =
            indexedGridData uSteps vSteps False False
    in
    fromOrientedFacesUnchecked
        (Array.map (\( u, v ) -> toVertex u v) verts)
        faces


indexedTube : Int -> Int -> (Int -> Int -> vertex) -> Mesh vertex
indexedTube uSteps vSteps toVertex =
    let
        ( verts, faces ) =
            indexedGridData uSteps vSteps False True
    in
    fromOrientedFacesUnchecked
        (Array.map (\( u, v ) -> toVertex u v) verts)
        faces


indexedRing : Int -> Int -> (Int -> Int -> vertex) -> Mesh vertex
indexedRing uSteps vSteps toVertex =
    let
        ( verts, faces ) =
            indexedGridData uSteps vSteps True True
    in
    fromOrientedFacesUnchecked
        (Array.map (\( u, v ) -> toVertex u v) verts)
        faces


indexedBall : Int -> Int -> (Int -> Int -> vertex) -> Mesh vertex
indexedBall uSteps vSteps toVertex =
    if vSteps < 2 then
        empty

    else
        let
            ( vertsTube, facesTube ) =
                if vSteps == 2 then
                    ( Array.initialize uSteps (\u -> ( u, 0 )), [] )

                else
                    indexedGridData uSteps (vSteps - 2) True False

            verts =
                vertsTube
                    |> Array.map (\( u, v ) -> ( u, v + 1 ))
                    |> Array.push ( 0, 0 )
                    |> Array.push ( 0, vSteps )

            bottom =
                Array.length vertsTube

            bottomCapFaces =
                List.range 0 (uSteps - 1)
                    |> List.map (\u -> [ u, bottom, u |> incrMod uSteps ])

            top =
                bottom + 1

            topRow =
                uSteps * (vSteps - 2)

            topCapFaces =
                List.range 0 (uSteps - 1)
                    |> List.map
                        (\u ->
                            [ topRow + u
                            , topRow + (u |> incrMod uSteps)
                            , top
                            ]
                        )
        in
        fromOrientedFacesUnchecked
            (Array.map (\( u, v ) -> toVertex u v) verts)
            (List.concat [ facesTube, bottomCapFaces, topCapFaces ])



-- Various helper functions, mostly for lists


when : Bool -> a -> a -> a
when cond trueVal falseVal =
    if cond then
        trueVal

    else
        falseVal


incrMod : Int -> Int -> Int
incrMod modulus i =
    (i + 1) |> modBy modulus


flip : (c -> b -> a) -> b -> c -> a
flip f x y =
    f y x


getAll : List Int -> Array a -> List a
getAll indices array =
    List.filterMap (\i -> Array.get i array) indices


traceCycle : a -> (a -> Maybe a) -> List a
traceCycle start advance =
    let
        step cycle current =
            case advance current of
                Just to ->
                    if to == start then
                        current :: cycle

                    else
                        step (current :: cycle) to

                Nothing ->
                    []
    in
    step [] start |> List.reverse


extractCycles :
    List comparable
    -> (comparable -> Maybe comparable)
    -> List (List comparable)
extractCycles items advance =
    let
        step cycles remaining =
            case remaining of
                first :: _ ->
                    let
                        cycle =
                            traceCycle first advance

                        purge =
                            Set.fromList cycle
                    in
                    step
                        (cycle :: cycles)
                        (List.filter (flip Set.member purge >> not) remaining)

                _ ->
                    cycles
    in
    step [] items


triangulate : List vertex -> List ( vertex, vertex, vertex )
triangulate corners =
    case corners of
        u :: v :: rest ->
            List.map2 (\r s -> ( u, r, s )) (v :: rest) rest

        _ ->
            []


cyclicPairs : List a -> List ( a, a )
cyclicPairs indices =
    case indices of
        first :: rest ->
            List.map2 Tuple.pair indices (rest ++ [ first ])

        _ ->
            []


canonicalCircular : List comparable -> List comparable
canonicalCircular list =
    List.range 0 (List.length list - 1)
        |> List.map (\k -> List.drop k list ++ List.take k list)
        |> List.minimum
        |> Maybe.withDefault list


hasDuplicates : List comparable -> Bool
hasDuplicates =
    let
        hasDupes xs =
            case xs of
                first :: second :: rest ->
                    first == second || hasDupes (second :: rest)

                _ ->
                    False
    in
    List.sort >> hasDupes
