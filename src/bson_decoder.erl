-module(bson_decoder).

%% high level functions
-export([decode/1]).
%% lower level functions
-export([
    decode_int32/1,
    decode_int64/1,
    decode_float64/1,
    decode_boolean/1,
    decode_string/1,
    decode_cstring/1,
    decode_null/1,
    decode_binary/1,
    decode_date/1,
    decode_regexp/1,
    decode_timestamp/1,
    decode_javascript/1,
    decode_objectid/1,
    decode_min_key/1,
    decode_max_key/1,
    decode_document/1,
    decode_array/1
]).

-include_lib("bson/include/bson.hrl").
-include("./bson.hrl").

%% @doc Returns an erlang term representation of
%% the BSON document type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode(Binary :: binary()) -> {map(), binary()}.
decode(<<?int32(Size), Remainder/binary>> = Binary) ->
    Size = erlang:byte_size(Binary),
    decode_document(Remainder, #{}).

%% === TYPE DECODER FUNCTIONS ===

%% @doc Returns an erlang term representation of
%% the BSON int32 type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_int32(Binary :: binary()) -> {integer(), binary()}.
decode_int32(<<?int32(Int), Remainder/binary>>) ->
    {Int, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON int64 type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_int64(Binary :: binary()) -> {integer(), binary()}.
decode_int64(<<?int64(Int), Remainder/binary>>) ->
    {Int, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON float64 type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_float64(Binary :: binary()) -> {integer(), binary()}.
decode_float64(<<?float64(Float), Remainder/binary>>) ->
    {Float, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON boolean type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_boolean(Binary :: binary()) -> {boolean(), binary()}.
decode_boolean(<<?uint8(Byte), Remainder/binary>>) ->
    {Byte =:= 1, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON string type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_string(Binary :: binary()) -> {binary(), binary()}.
decode_string(<<?int32(Size), Binary/binary>>) ->
    Length = Size - 1,
    <<String:Length/binary, 0:8, Remainder/binary>> = Binary,
    {String, Remainder}.

-spec decode_cstring(Binary :: binary()) -> {binary(), binary()}.
decode_cstring(Binary) ->
    read_terminal(Binary).

%% @doc Returns an erlang term representation of
%% the BSON null type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_null(Binary :: binary()) -> {null, binary()}.
decode_null(Remainder) ->
    {null, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON binary type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_binary(Binary :: binary()) -> {bson:bin(), binary()}.
decode_binary(<<?int32(Size), ?uint8(Tag), Binary/binary>>) ->
    Type = case Tag of
        ?BSON_SUBTYPE_BINARY -> binary;
        ?BSON_SUBTYPE_FUNCTION -> function;
        ?BSON_SUBTYPE_UUID -> uuid;
        ?BSON_SUBTYPE_MD5 -> md5;
        ?BSON_SUBTYPE_USER_DEFINED -> '$$'
    end,
    <<Bytes:Size/binary, Remainder/binary>> = Binary,
    {{Type, Bytes}, Remainder}.

%% @doc Returns an erlang timestamp representation of
%% the BSON UTC date type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_date(Binary :: binary()) -> {erlang:timestamp(), binary()}.
decode_date(<<?int64(Millisecs), Remainder/binary>>) ->
    Megasecs = Millisecs div 1000000000,
    Secs = (Millisecs div 1000) rem 1000000,
    Microsecs = Millisecs * 1000 rem 1000000,
    {{Megasecs, Secs, Microsecs}, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON regexp type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_regexp(Binary :: binary()) -> {bson:regexp(), binary()}.
decode_regexp(Binary) ->
    {Regexp, Rest} = decode_cstring(Binary),
    {Opts, Remainder} = decode_cstring(Rest),
    {{regexp, Regexp, Opts}, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON timestamp type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_timestamp(Binary :: binary()) -> {bson:timestamp(), binary()}.
decode_timestamp(<<?uint32(Timestamp), ?uint32(Id), Remainder/binary>>) ->
    {{timestamp, Timestamp, Id}, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON javascript code type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_javascript(Binary :: binary()) -> {bson:javascript(), binary()}.
decode_javascript(Binary) ->
    {Code, Remainder} = decode_string(Binary),
    {{javascript, Code}, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON object id type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_objectid(Binary :: binary()) -> {bson:objectid(), binary()}.
decode_objectid(<<Id:12/binary, Remainder/binary>>) ->
    {{Id}, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON min key type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_min_key(Binary :: binary()) -> {bson:min_key(), binary()}.
decode_min_key(Remainder) ->
    {'MIN_KEY', Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON max key type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_max_key(Binary :: binary()) -> {bson:max_key(), binary()}.
decode_max_key(Remainder) ->
    {'MAX_KEY', Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON document type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_document(Binary :: binary()) -> {map(), binary()}.
decode_document(Binary) ->
    decode_document(Binary, #{}).

%% @doc Returns an erlang term representation of
%% the BSON array type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec decode_array(Binary :: binary()) -> {list(), binary()}.
decode_array(Binary) ->
    decode_array(Binary, []).

%% === UTILITY DECODER FUNCTIONS ===

-spec decode_document(Binary :: binary(), Acc :: map()) -> {map(), binary()}.
decode_document(<<?BSON_EOT, Remainder/binary>>, Acc) ->
    {Acc, Remainder};
decode_document(<<?uint8(Type), Binary/binary>>, Acc) ->
    {Key, Value, Remainder} = decode_field(Type, Binary),
    decode_document(Remainder, Acc#{Key => Value}).

-spec decode_array(Binary :: binary(), Acc :: list()) -> {list(), binary()}.
decode_array(<<?BSON_EOT, Remainder/binary>>, Acc) ->
    {Acc, Remainder};
decode_array(<<?uint8(Type), Binary/binary>>, Acc) ->
    {Key, Value, Remainder} = decode_field(Type, Binary),
    Length = erlang:binary_to_integer(Key),
    Length = erlang:length(Acc),
    decode_array(Remainder, Acc ++ [Value]).

-spec decode_field(Type :: integer(), Binary :: binary()) -> {binary(), term(), binary()}.
decode_field(Type, Binary) ->
    {Key, Rest} = decode_key(Binary),
    {Value, Remainder} = decode_value(Type, Rest),
    {Key, Value, Remainder}.

-spec decode_key(Binary :: binary()) -> {binary(), binary()}.
decode_key(Binary) ->
    decode_cstring(Binary).

-spec decode_value(Type :: integer(), Binary :: binary()) -> {term(), binary()}.
decode_value(?BSON_INT32, Binary) -> decode_int32(Binary);
decode_value(?BSON_INT64, Binary) -> decode_int64(Binary);
decode_value(?BSON_FLOAT64, Binary) -> decode_float64(Binary);
decode_value(?BSON_BOOLEAN, Binary) -> decode_boolean(Binary);
decode_value(?BSON_STRING, Binary) -> decode_string(Binary);
decode_value(?BSON_NULL, Binary) -> decode_null(Binary);
decode_value(?BSON_BINARY, Binary) -> decode_binary(Binary);
decode_value(?BSON_DATE, Binary) -> decode_date(Binary);
decode_value(?BSON_REGEXP, Binary) -> decode_regexp(Binary);
decode_value(?BSON_TIMESTAMP, Binary) -> decode_timestamp(Binary);
decode_value(?BSON_JAVASCRIPT, Binary) -> decode_javascript(Binary);
decode_value(?BSON_OBJECTID, Binary) -> decode_objectid(Binary);
decode_value(?BSON_DOCUMENT, <<?int32(_), Binary/binary>>) -> decode_document(Binary);
decode_value(?BSON_ARRAY, <<?int32(_), Binary/binary>>) -> decode_array(Binary);
decode_value(?MIN_KEY, Binary) -> decode_min_key(Binary);
decode_value(?MAX_KEY, Binary) -> decode_max_key(Binary).

-spec read_terminal(Binary :: binary()) -> {binary(), binary()}.
read_terminal(Binary) ->
    read_terminal(Binary, <<>>).

-spec read_terminal(Binary :: binary(), Acc :: binary()) -> {binary(), binary()}.
read_terminal(<<?BSON_EOT, Remainder/binary>>, Acc) ->
    {Acc, Remainder};
read_terminal(<<Bit:1/binary, Remainder/binary>>, Acc) ->
    read_terminal(Remainder, <<Acc/binary, Bit/binary>>).
