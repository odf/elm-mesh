module Mesh.Topology exposing
    ( Mesh
    , combine
    , edgeIndices
    , edgeVertices
    , empty
    , faceIndices
    , faceVertices
    , fromOrientedFaces
    , fromTriangularMesh
    , mapVertices
    , neighborIndices
    , neighborVertices
    , smoothSubdivision
    , subdivision
    , toTriangularMesh
    , vertex
    , vertices
    , withNormals
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Set
import TriangularMesh exposing (TriangularMesh)


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
    if Set.size orientedEdgeSet < List.length orientedEdges then
        Err "each oriented edge must be unique"

    else if List.any (opposite >> seen >> not) orientedEdges then
        Err "each oriented edge must have a reverse"

    else
        Ok (fromOrientedFacesUnchecked vertexData faceLists)


vertices : Mesh vertex -> Array vertex
vertices (Mesh verts _) =
    verts


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


faceIndices : Mesh vertex -> List (List Int)
faceIndices (Mesh verts mesh) =
    Array.toList mesh.alongFace
        |> List.map (\start -> verticesInFace start (Mesh verts mesh))
        |> List.map canonicalCircular


faceVertices : Mesh vertex -> List (List vertex)
faceVertices mesh =
    let
        toFace =
            List.filterMap (\i -> vertex i mesh)
    in
    List.map toFace (faceIndices mesh)


edgeIndices : Mesh vertex -> List ( Int, Int )
edgeIndices (Mesh _ mesh) =
    Dict.keys mesh.next |> List.filter (\( from, to ) -> from <= to)


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


subdivision : (List vertex -> vertex) -> Mesh vertex -> Mesh vertex
subdivision composeFn (Mesh verts mesh) =
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


smoothSubdivision :
    (vertex -> Bool)
    -> (vertex -> Vec3)
    -> (List vertex -> Vec3 -> vertex)
    -> Mesh vertex
    -> Mesh vertex
smoothSubdivision isFixed vertexPosition toOutputVertex meshIn =
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
                |> subdivision centroid

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
