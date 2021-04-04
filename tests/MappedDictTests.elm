module MappedDictTests exposing
    ( empty
    , fromList
    )

import Expect
import MappedDict as Dict
import Test exposing (Test)


empty : Test
empty =
    Test.test "empty"
        (\() ->
            Dict.empty identity
                |> Dict.keys
                |> Expect.equal []
        )


fromList : Test
fromList =
    Test.test "fromList"
        (\() ->
            Dict.fromList (\k -> k * k) [ ( 1, 'a' ), ( 2, 'b' ), ( 3, 'c' ) ]
                |> Expect.all
                    [ Dict.keys >> Expect.equal [ 1, 2, 3 ]
                    , Dict.values >> Expect.equal [ 'a', 'b', 'c' ]
                    , Dict.toList
                        >> Expect.equal [ ( 1, 'a' ), ( 2, 'b' ), ( 3, 'c' ) ]
                    , Dict.get 2 >> Expect.equal (Just 'b')
                    , Dict.get 0 >> Expect.equal Nothing
                    ]
        )
