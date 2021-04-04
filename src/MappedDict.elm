module MappedDict exposing
    ( Dict
    , empty
    , fromList
    , get
    , keys
    , member
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


member : k -> Dict comparable k v -> Bool
member key (MappedDict makeKey dict) =
    Dict.member (makeKey key) dict
