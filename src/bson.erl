-module(bson).

%% api functions
-export([decode/1, encode/1, construct/1, destruct/2]).
%% utility functions
-export([objectid_to_binary/1, binary_to_objectid/1]).
%% helper functions
-export([loop/2]).

-type document() :: map() | list().
-type array() :: list().
-type objectid() :: {binary()}.
-type datetime() :: erlang:timestamp().
-type timestamp() :: {timestamp, integer()}.
-type javascript() :: {javascript, binary()}.
-type regexp() :: {regexp, {binary(), binary()}}.
-type bin() :: {binary | function | uuid | md5 | '$$', binary()}.
-type min_key() :: 'MIN_KEY'.
-type max_key() :: 'MAX_KEY'.
-export_type([document/0, array/0]).
-export_type([objectid/0, datetime/0, timestamp/0]).
-export_type([bin/0, regexp/0, javascript/0, min_key/0, max_key/0]).

%% @doc Returns `{Document, Remainder}' based on `Payload'
%% where `Document' is the decoded BSON document (`bson:document()')
%% and `Remainder' is the bytes remaining from `Payload'.
-spec decode(Payload :: binary()) -> {bson:document(), binary()}.
decode(Payload) -> bson_decoder:document(Payload).

%% @doc Returns the BSON representation of `Document'.
-spec encode(Document :: bson:document()) -> binary().
encode(Document) -> bson_encoder:document(Document).

%% @doc Constructs a BSON binary based on `Spec'
%% where `Spec' is a list of tuples such as `{Encoding, Value}'
%% where `Encoding' is a valid `bson_encoder' function
%% and `Value' is the value to encode using the specified encoding.
-spec construct(Spec :: list()) -> binary().
construct(Spec) -> bson_encoder:struct(Spec).

%% @doc Desctructs a BSON binary (`Payload') based on `Spec'
%% where `Spec' is a list of atoms, each representing a valid
%% `bson_decoder' function.
-spec destruct(Spec :: list(), Payload :: binary()) -> list().
destruct(Spec, Payload) -> bson_decoder:struct(Spec, Payload).

%% @doc Returns the `bson:objectid()' representation of `Id'
%% where `Id' is a BSON ObjectId in `hexadecimal' string format.
-spec binary_to_objectid(binary()) -> objectid().
binary_to_objectid(Id) ->
    bson:loop(fun
        ({<<>>, Acc}) -> {false, {Acc}};
        ({<<Hex:2/binary, Rest/binary>>, Acc}) ->
            Chunk = erlang:binary_to_integer(Hex, 16),
            {true, {Rest, <<Acc/binary, Chunk>>}}
    end, {Id, <<>>}).

%% @doc Returns the `hexadecimal' string representation of `Id'
%% where `Id' is a `bson:objectid()'.
-spec objectid_to_binary(objectid()) -> binary().
objectid_to_binary({Id}) ->
    bson:loop(fun
        ({<<>>, Acc}) -> {false, string:lowercase(Acc)};
        ({<<Chunk, Rest/binary>>, Acc}) ->
            Hex = erlang:integer_to_binary(Chunk, 16),
            Pad = case Hex of <<_>> -> <<"0">>; _ -> <<>> end,
            {true, {Rest, <<Acc/binary, Pad/binary, Hex/binary>>}}
    end, {Id, <<>>}).

%% @hidden
-spec loop(Function :: fun(), State :: term()) -> term().
loop(Function, State) ->
    case Function(State) of
        {true, Result} -> loop(Function, Result);
        {false, Result} -> Result
    end.
