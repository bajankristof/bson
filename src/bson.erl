-module(bson).

%% api functions
-export([decode/1, encode/1, construct/1, destruct/2]).
%% utility functions
-export([objectid_to_binary/1, binary_to_objectid/1]).
%% helper functions
-export([loop/2]).

-type objectid() :: {binary()}.
-type datetime() :: erlang:timestamp().
-type timestamp() :: {timestamp, integer()}.
-type javascript() :: {javascript, binary()}.
-type regexp() :: {regexp, {binary(), binary()}}.
-type bin() :: {binary | function | uuid | md5 | '$$', binary()}.
-type min_key() :: 'MIN_KEY'.
-type max_key() :: 'MAX_KEY'.
-export_type([objectid/0, datetime/0, timestamp/0]).
-export_type([bin/0, regexp/0, javascript/0, min_key/0, max_key/0]).

-spec decode(Payload :: binary()) -> {map(), binary()}.
decode(Payload) -> bson_decoder:document(Payload).

-spec encode(Document :: map() | list()) -> binary().
encode(Document) -> bson_encoder:document(Document).

-spec construct(Spec :: list()) -> binary().
construct(Spec) -> bson_encoder:struct(Spec).

-spec destruct(Spec :: list(), Payload :: binary()) -> list().
destruct(Spec, Payload) -> bson_decoder:struct(Spec, Payload).

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

-spec loop(Function :: fun(), State :: term()) -> term().
loop(Function, State) ->
    case Function(State) of
        {true, Result} -> loop(Function, Result);
        {false, Result} -> Result
    end.
