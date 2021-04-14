module Mesh exposing
    ( Mesh
    , empty
    , fromTriangularMesh, fromOrientedFaces
    , combine
    , vertices, vertex, faceIndices, faceVertices
    , edgeIndices, edgeVertices, neighborIndices, neighborVertices
    , mapVertices, withNormals, subdivide, subdivideSmoothly
    , toTriangularMesh
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

@docs fromTriangularMesh, fromOrientedFaces


# Combining meshes

@docs combine


# Properties

@docs vertices, vertex, faceIndices, faceVertices
@docs edgeIndices, edgeVertices, neighborIndices, neighborVertices


# Transformations

@docs mapVertices, withNormals, subdivide, subdivideSmoothly


# Exporting

@docs toTriangularMesh

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set
import TriangularMesh exposing (TriangularMesh)


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

Right now it is also not allowed to defined a mesh with a boundary.

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
        , next : Dict OrientedEdge OrientedEdge
        , toFace : Dict OrientedEdge Int
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
        , next = Dict.empty
        , toFace = Dict.empty
        }


fromOrientedFacesUnchecked : Array vertex -> List (List Int) -> Mesh vertex
fromOrientedFacesUnchecked vertexData faceLists =
    let
        orientedEdgeLists =
            List.map cyclicPairs faceLists

        orientedEdges =
            List.concat orientedEdgeLists

        next =
            List.concatMap cyclicPairs orientedEdgeLists |> Dict.fromList

        fromVertex =
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

        fromFace =
            Dict.toList toFace
                |> List.map (\( key, val ) -> ( val, key ))
                |> Dict.fromList
                |> Dict.values
                |> Array.fromList
    in
    Mesh vertexData
        { atVertex = fromVertex
        , alongFace = fromFace
        , next = next
        , toFace = toFace
        }


{-| Create a mesh from an array of vertices and list of face indices. For
example, to construct a square pyramid where `a` is the front right corner,
`b` is the front left corner, `c` is the back left corner, `d` is the back
right corner and `e` is the apex:

    vertices =
        Array.fromList [ a, b, c, d, e ]

    faceIndices =
        [ [ 0, 1, 2, 3 ], [ 0, 4, 1 ], [ 1, 4, 2 ], [ 2, 4, 3 ], [ 3, 4, 0 ] ]

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
        orientedEdges =
            List.map cyclicPairs faceLists |> List.concat

        orientedEdgeSet =
            Set.fromList orientedEdges

        seen e =
            Set.member e orientedEdgeSet
    in
    -- TODO check that all vertex indices used are in the array range
    -- TODO check that each vertex belongs to at least one face
    if Set.size orientedEdgeSet < List.length orientedEdges then
        Err "each oriented edge must be unique"

    else if List.any (opposite >> seen >> not) orientedEdges then
        Err "each oriented edge must have a reverse"

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

    TriangularMesh.vertex 1 pyramid
    --> Just b

    TriangularMesh.vertex 5 pyramid
    --> Nothing

-}
vertex : Int -> Mesh vertex -> Maybe vertex
vertex index mesh =
    Array.get index (vertices mesh)


verticesInFace : OrientedEdge -> Mesh vertex -> List Int
verticesInFace start (Mesh _ mesh) =
    let
        step vertsIn current =
            let
                vertsOut =
                    Tuple.first current :: vertsIn
            in
            case Dict.get current mesh.next of
                Just next ->
                    if next == start then
                        vertsOut

                    else
                        step vertsOut next

                Nothing ->
                    []
    in
    step [] start |> List.reverse


{-| Get the faces of a mesh as lists of vertex indices. Each face is
normalized so that its smallest vertex index comes first.

    Mesh.faceIndices pyramid |> List.sort
    --> [ [ 0, 1, 2, 3 ], [ 0, 3, 4 ], [ 0, 4, 1 ], [ 1, 4, 2 ], [ 2, 4, 3 ] ]

-}
faceIndices : Mesh vertex -> List (List Int)
faceIndices (Mesh verts mesh) =
    Array.toList mesh.alongFace
        |> List.map (\start -> verticesInFace start (Mesh verts mesh))
        |> List.map canonicalCircular


{-| Get the faces of a mesh as lists of vertices. The ordering is the same as
in `faceIndices`.

    Mesh.faceVertices pyramid |> List.sort
    --> [ [ a, b, c, d ], [ a, d, e ], [ a, e, b ], [ b, e, c ], [ c, e, d ] ]

-}
faceVertices : Mesh vertex -> List (List vertex)
faceVertices mesh =
    let
        toFace =
            List.filterMap (\i -> vertex i mesh)
    in
    List.map toFace (faceIndices mesh)


{-| Get all of the edges of a mesh as pairs of vertex indices. Each edge will
only be returned once, with the lower-index vertex listed first.

    Mesh.edgeIndices pyramid |> List.sort
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
    Dict.keys mesh.next |> List.filter (\( from, to ) -> from <= to)


{-| Get all of the edges of a mesh as pairs of vertices. Each edge will
only be returned once, with the lower-index vertex listed first.

    Mesh.edgeVertices pyramid |> List.sort
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
    let
        step vertsIn current =
            let
                vertsOut =
                    Tuple.second current :: vertsIn
            in
            case Dict.get (opposite current) mesh.next of
                Just next ->
                    if next == start then
                        vertsOut

                    else
                        step vertsOut next

                Nothing ->
                    []
    in
    step [] start


neighborIndices : Mesh vertex -> Array (List Int)
neighborIndices (Mesh verts mesh) =
    Array.toList mesh.atVertex
        |> List.map (\start -> vertexNeighbors start (Mesh verts mesh))
        |> List.map canonicalCircular
        |> Array.fromList


neighborVertices : Mesh vertex -> Array (List vertex)
neighborVertices mesh =
    neighborIndices mesh |> Array.map (List.filterMap (\i -> vertex i mesh))


toTriangularMesh : Mesh vertex -> TriangularMesh vertex
toTriangularMesh mesh =
    TriangularMesh.indexed
        (vertices mesh)
        (faceIndices mesh |> List.concatMap triangulate)


fromTriangularMesh : TriangularMesh vertex -> Result String (Mesh vertex)
fromTriangularMesh trimesh =
    TriangularMesh.faceIndices trimesh
        |> List.map (\( u, v, w ) -> [ u, v, w ])
        |> fromOrientedFaces (TriangularMesh.vertices trimesh)


mapVertices : (a -> b) -> Mesh a -> Mesh b
mapVertices fn (Mesh verts mesh) =
    Mesh (Array.map fn verts) mesh


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


withNormals : (a -> Vec3) -> (a -> Vec3 -> b) -> Mesh a -> Mesh b
withNormals toPositionIn toVertexOut (Mesh verts mesh) =
    let
        neighbors =
            neighborVertices (Mesh verts mesh)

        mapVertex idx v =
            let
                p =
                    toPositionIn v
            in
            Array.get idx neighbors
                |> Maybe.withDefault []
                |> List.map (toPositionIn >> Vec3.sub p)
                |> cyclicPairs
                |> List.map (\( a, b ) -> Vec3.cross a b)
                |> List.foldl Vec3.add (vec3 0 0 0)
                |> Vec3.normalize
                |> toVertexOut v

        verticesOut =
            verts
                |> Array.toList
                |> List.indexedMap mapVertex
                |> Array.fromList
    in
    Mesh verticesOut mesh


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
            List.map subFace (Dict.keys mesh.next)
    in
    fromOrientedFacesUnchecked vertsOut facesOut


getAll : List Int -> Array a -> List a
getAll indices array =
    List.filterMap (\i -> Array.get i array) indices


centroid : List Vec3 -> Vec3
centroid points =
    List.foldl Vec3.add (vec3 0 0 0) points
        |> Vec3.scale (1 / toFloat (List.length points))


subdivideSmoothly :
    (vertex -> Bool)
    -> (vertex -> Vec3)
    -> (List vertex -> Vec3 -> vertex)
    -> Mesh vertex
    -> Mesh vertex
subdivideSmoothly isFixed vertexPosition toOutputVertex meshIn =
    let
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
                    vertex i meshSub |> Maybe.withDefault (vec3 0 0 0)

                neighbors =
                    Array.get i neighborsSub |> Maybe.withDefault []

                nrNeighbors =
                    List.length neighbors

                centroidOfEdgeCenters =
                    neighbors
                        |> List.filterMap (\k -> vertex k meshSub)
                        |> centroid

                centroidOfEdgePoints =
                    neighbors
                        |> List.map (\k -> k - nrVertices)
                        |> List.filterMap (\k -> Array.get k edgePointPos)
                        |> centroid
            in
            if
                Array.get i verticesIn
                    |> Maybe.map isFixed
                    |> Maybe.withDefault False
            then
                makeOutputVertex [ i ] posIn

            else
                Vec3.scale (toFloat nrNeighbors - 3) posIn
                    |> Vec3.add centroidOfEdgeCenters
                    |> Vec3.add (Vec3.scale 2 centroidOfEdgePoints)
                    |> Vec3.scale (1 / toFloat nrNeighbors)
                    |> makeOutputVertex [ i ]

        vertexPoints =
            List.range 0 (nrVertices - 1) |> List.map makeVertexPoint

        verticesOut =
            Array.fromList (vertexPoints ++ edgePoints ++ facePoints)
    in
    fromOrientedFacesUnchecked verticesOut subFaceIndices



-- List helpers


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
