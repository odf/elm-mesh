module DictTests exposing
    ( empty
    , fromList
    , insert
    , wrappedKeys
    )

import Expect
import Mesh.Dict as Dict
import Test exposing (Test)


empty : Test
empty =
    Test.test "empty"
        (\() ->
            Dict.empty identity
                |> Expect.all
                    [ Dict.keys >> Expect.equal []
                    , Dict.values >> Expect.equal []
                    , Dict.toList >> Expect.equal []
                    ]
        )


fromList : Test
fromList =
    Test.test "fromList"
        (\() ->
            Dict.fromList identity [ ( 1, 'a' ), ( 2, 'b' ), ( 3, 'c' ) ]
                |> Expect.all
                    [ Dict.keys >> Expect.equal [ 1, 2, 3 ]
                    , Dict.values >> Expect.equal [ 'a', 'b', 'c' ]
                    , Dict.toList
                        >> Expect.equal [ ( 1, 'a' ), ( 2, 'b' ), ( 3, 'c' ) ]
                    , Dict.get 2 >> Expect.equal (Just 'b')
                    , Dict.get 0 >> Expect.equal Nothing
                    , Dict.member 2 >> Expect.equal True
                    , Dict.member 4 >> Expect.equal False
                    ]
        )


type Wrap
    = Wrap Int


unwrap : Wrap -> Int
unwrap (Wrap n) =
    n


wrappedKeys : Test
wrappedKeys =
    Test.test "wrappedKeys"
        (\() ->
            Dict.fromList
                unwrap
                [ ( Wrap 1, 'a' ), ( Wrap 2, 'b' ), ( Wrap 3, 'c' ) ]
                |> Expect.all
                    [ Dict.keys >> Expect.equal [ Wrap 1, Wrap 2, Wrap 3 ]
                    , Dict.values >> Expect.equal [ 'a', 'b', 'c' ]
                    , Dict.toList
                        >> Expect.equal
                            [ ( Wrap 1, 'a' ), ( Wrap 2, 'b' ), ( Wrap 3, 'c' ) ]
                    , Dict.get (Wrap 2) >> Expect.equal (Just 'b')
                    , Dict.get (Wrap 0) >> Expect.equal Nothing
                    , Dict.member (Wrap 2) >> Expect.equal True
                    , Dict.member (Wrap 4) >> Expect.equal False
                    ]
        )


insert : Test
insert =
    Test.test "insert"
        (\() ->
            Dict.empty unwrap
                |> Dict.insert (Wrap 2) 'b'
                |> Dict.insert (Wrap 1) 'a'
                |> Expect.all
                    [ Dict.get (Wrap 1) >> Expect.equal (Just 'a')
                    , Dict.get (Wrap 2) >> Expect.equal (Just 'b')
                    , Dict.get (Wrap 3) >> Expect.equal Nothing
                    , Dict.keys >> Expect.equal [ Wrap 1, Wrap 2 ]
                    , Dict.values >> Expect.equal [ 'a', 'b' ]
                    , Dict.toList
                        >> Expect.equal [ ( Wrap 1, 'a' ), ( Wrap 2, 'b' ) ]
                    ]
        )