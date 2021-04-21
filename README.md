# elm-mesh

This [Elm](http://elm-lang.org) package allows you to create and manipulate
indexed meshes with arbitrary face sizes. It was inspired by
[elm-triangular-mesh](http://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/1.1.0/TriangularMesh)
and generally works in a very similar way. A mesh contains an array of vertices
that can be of any type; the faces of a mesh are defined by lists of integer
indices specifying which vertices makes up the face.

There are however two important differences:
  * Faces can have three or more vertices, not just three.
  * Each edge that is not on the boundary of the mesh must appear in exactly two faces, and in opposite directions.

The second property is best explained by an example:

```elm
import Mesh exposing (Mesh)
import Array

mesh : Mesh ( Float, Float )
mesh =
    let
        vertices =
            Array.fromList
                [ ( 0, 0 )
                , ( 4, 0 )
                , ( 4, 3 )
                , ( 0, 3 )
                ]

        faceIndices =
            [ [ 0, 1, 2, 3 ]
            , [ 3, 2, 1, 0 ]
            ]
    in
    Mesh.fromOrientedFaces vertices faceIndices
```

This creates a mesh that contains two rectangles put together back to back
like a pillow. The first face, `[ 0, 1, 2, 3 ]`, contains four directed
edges described by the index pairs `( 0, 1 )`, `( 1, 2 )`, `( 2, 3 )`, and
`( 3, 0 )`, the last one connecting the final vertex back to the first one
and thus closing up the face. Similarly, the second face contains the
directed edges `( 3, 2 )`, `( 2, 1 )`, `( 1, 0 )`, and `( 0, 3 )`. Each pair
in the first list is matched by one in the second list with its first and
second value, or in other words its beginning and end, flipped.

We can then retrieve the list of (undirected) edges described by their
indices like this
```elm
Mesh.edgeIndices mesh
--> [ ( 0, 1 )
--> , ( 0, 3 )
--> , ( 1, 2 )
--> , ( 2, 3 )
--> ]
```
or as vertices like this
```elm
Mesh.edgeVertices mesh
--> [ ( ( 0, 0 ), ( 4, 0 ) )
--> , ( ( 0, 0 ), ( 0, 3 ) )
--> , ( ( 4, 0 ), ( 4, 3 ) )
--> , ( ( 4, 3 ), ( 0, 3 ) )
--> ]
```
