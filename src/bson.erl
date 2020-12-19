-module(bson).

-export([
    decode/1,
    encode/1
]).

-export([
    objectid_to_binary/1,
    binary_to_objectid/1
]).

-type objectid() :: {binary()}.
-type date() :: erlang:timestamp().
-type bin() :: {binary | function | uuid | md5, binary()}.
-type regexp() :: {regexp, binary(), binary()}.
-type timestamp() :: {timestamp, integer(), integer()}.
-type javascript() :: {javascript, binary()}.
-type min_key() :: 'MIN_KEY'.
-type max_key() :: 'MAX_KEY'.
-export_type([
    objectid/0,
    date/0,
    bin/0,
    regexp/0,
    timestamp/0,
    javascript/0,
    min_key/0,
    max_key/0
]).

-spec decode(Binary :: binary()) -> {map(), binary()}.
decode(Binary) -> bson_decoder:decode(Binary).

-spec encode(Document :: map()) -> binary().
encode(Document) -> bson_encoder:encode(Document).

-spec binary_to_objectid(binary()) -> objectid().
binary_to_objectid(Id) ->
    Result = lists:foldr(fun
        (Pad, [{Hex}|Acc]) ->
            [erlang:binary_to_integer(<<Pad, Hex>>, 16)] ++ Acc;
        (Hex, Acc) -> [{Hex}|Acc]
    end, [], erlang:binary_to_list(Id)),
    {erlang:list_to_binary(Result)}.

-spec objectid_to_binary(objectid()) -> binary().
objectid_to_binary({Id}) ->
    Result = lists:foldl(fun (Byte, Acc) ->
        Hex = erlang:integer_to_list(Byte, 16),
        Pad = case Hex of [_] -> "0"; _ -> [] end,
        lists:append([Acc, Pad, string:lowercase(Hex)])
    end, [], erlang:binary_to_list(Id)),
    erlang:list_to_binary(Result).
