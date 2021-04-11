# elm-mesh

This [Elm](http://elm-lang.org) package allows you to create and manipulate
indexed meshes with arbitrary face sizes. It was inspired by
[elm-triangular-mesh](http://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/1.0.0/TriangularMesh)
and generally works in a very similar way. A mesh contains an array of vertices
that can be of any type; the faces of a mesh are defined by lists of integer
indices specifying which vertices makes up the face.

There are however a few important differences:
  * Faces can have three or more vertices, not just three.
  * Each edge must appear in exactly two faces.
  * Faces must be oriented consistently.

The last two properties are best explained by an example, here a mesh that
contains two rectangles put together back to back like a pillow:

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

            
