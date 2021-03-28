module TopologyTests exposing
    ( orientationMismatch
    , goodFaceList
    , unpairedOrientedEdge
    )

import Expect
import Test exposing (Test)
import Topology


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


goodFaceList : Test
goodFaceList =
    Test.test "goodFaceList"
        (\() ->
            octahedronFaces
                |> Topology.fromOrientedFaces
                |> Expect.ok
        )


unpairedOrientedEdge : Test
unpairedOrientedEdge =
    Test.test "unpairedOrientedEdge"
        (\() ->
            octahedronFaces
                |> List.drop 1
                |> Topology.fromOrientedFaces
                |> Expect.err
        )


orientationMismatch : Test
orientationMismatch =
    Test.test "orientationMismatch"
        (\() ->
            octahedronFaces
                |> List.drop 1
                |> (::) [ 0, 2, 1 ]
                |> Topology.fromOrientedFaces
                |> Expect.err
        )
