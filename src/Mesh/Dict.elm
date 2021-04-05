module Mesh.Dict exposing
    ( Dict
    , andThenGetIn
    , empty
    , fromList
    , get
    , getIn
    , keys
    , member
    , reverse
    , toList
    , values
    )

import Dict


type Dict comparable k v
    = MappedDict (k -> comparable) (Dict.Dict comparable ( k, v ))


empty : (k -> comparable) -> Dict comparable k v
empty makeKey =
    MappedDict makeKey Dict.empty


fromList : (k -> comparable) -> List ( k, v ) -> Dict comparable k v
fromList makeKey keyValuePairs =
    keyValuePairs
        |> List.map (\( k, v ) -> ( makeKey k, ( k, v ) ))
        |> Dict.fromList
        |> MappedDict makeKey


toList : Dict comparable k v -> List ( k, v )
toList (MappedDict _ dict) =
    Dict.values dict


keys : Dict comparable k v -> List k
keys dict =
    toList dict |> List.map Tuple.first


values : Dict comparable k v -> List v
values dict =
    toList dict |> List.map Tuple.second


get : k -> Dict comparable k v -> Maybe v
get key (MappedDict makeKey dict) =
    Dict.get (makeKey key) dict |> Maybe.map Tuple.second


getIn : Dict comparable k v -> k -> Maybe v
getIn dict key =
    get key dict


andThenGetIn : Dict comparable k v -> Maybe k -> Maybe v
andThenGetIn =
    getIn >> Maybe.andThen


reverse : (v -> comparable2) -> Dict comparable1 k v -> Dict comparable2 v k
reverse makeKey =
    toList >> List.map (\( k, v ) -> ( v, k )) >> fromList makeKey


member : k -> Dict comparable k v -> Bool
member key (MappedDict makeKey dict) =
    Dict.member (makeKey key) dict
