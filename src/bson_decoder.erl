-module(bson_decoder).

-export([process/1]).

-include_lib("bson/include/bson.hrl").
-include("./bson.hrl").

%% @doc Returns an erlang term representation of
%% the BSON document type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process(Binary :: binary()) -> {map(), binary()}.
process(<<?int32(Size), Remainder/binary>> = Binary) ->
    Size = erlang:byte_size(Binary),
    process_document(Remainder, #{}).

%% === TYPE DECODER FUNCTIONS ===

%% @doc Returns an erlang term representation of
%% the BSON int32 type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_int32(Binary :: binary()) -> {integer(), binary()}.
process_int32(<<?int32(Int), Remainder/binary>>) ->
    {Int, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON int64 type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_int64(Binary :: binary()) -> {integer(), binary()}.
process_int64(<<?int64(Int), Remainder/binary>>) ->
    {Int, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON float64 type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_float64(Binary :: binary()) -> {integer(), binary()}.
process_float64(<<?float64(Float), Remainder/binary>>) ->
    {Float, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON boolean type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_boolean(Binary :: binary()) -> {boolean(), binary()}.
process_boolean(<<?uint8(Byte), Remainder/binary>>) ->
    {Byte =:= 1, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON string type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_string(Binary :: binary()) -> {binary(), binary()}.
process_string(<<?int32(Size), Binary/binary>>) ->
    Length = Size - 1,
    <<String:Length/binary, 0:8, Remainder/binary>> = Binary,
    {String, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON null type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_null(Binary :: binary()) -> {null, binary()}.
process_null(Remainder) ->
    {null, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON binary type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_binary(Binary :: binary()) -> {bson:bin(), binary()}.
process_binary(<<?int32(Size), ?uint8(Tag), Binary/binary>>) ->
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
-spec process_date(Binary :: binary()) -> {erlang:timestamp(), binary()}.
process_date(<<?int64(Millisecs), Remainder/binary>>) ->
    Megasecs = Millisecs div 1000000000,
    Secs = (Millisecs div 1000) rem 1000000,
    Microsecs = Millisecs * 1000 rem 1000000,
    {{Megasecs, Secs, Microsecs}, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON regexp type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_regexp(Binary :: binary()) -> {bson:regexp(), binary()}.
process_regexp(Binary) ->
    {Regexp, Rest} = read_terminal(Binary),
    {Opts, Remainder} = read_terminal(Rest),
    {{regexp, Regexp, Opts}, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON timestamp type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_timestamp(Binary :: binary()) -> {bson:timestamp(), binary()}.
process_timestamp(<<?uint32(Timestamp), ?uint32(Id), Remainder/binary>>) ->
    {{timestamp, Timestamp, Id}, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON javascript code type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_javascript(Binary :: binary()) -> {bson:javascript(), binary()}.
process_javascript(Binary) ->
    {Code, Remainder} = process_string(Binary),
    {{javascript, Code}, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON object id type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_objectid(Binary :: binary()) -> {bson:objectid(), binary()}.
process_objectid(<<Id:12/binary, Remainder/binary>>) ->
    {{Id}, Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON min key type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_min_key(Binary :: binary()) -> {bson:min_key(), binary()}.
process_min_key(Remainder) ->
    {'MIN_KEY', Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON max key type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_max_key(Binary :: binary()) -> {bson:max_key(), binary()}.
process_max_key(Remainder) ->
    {'MAX_KEY', Remainder}.

%% @doc Returns an erlang term representation of
%% the BSON document type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_document(Binary :: binary()) -> {map(), binary()}.
process_document(Binary) ->
    process_document(Binary, #{}).

%% @doc Returns an erlang term representation of
%% the BSON array type based on `Binary`.
%% (The second element of the tuple are the remaining bits of the operation.)
-spec process_array(Binary :: binary()) -> {list(), binary()}.
process_array(Binary) ->
    process_array(Binary, []).

%% === UTILITY DECODER FUNCTIONS ===

-spec process_document(Binary :: binary(), Acc :: map()) -> {map(), binary()}.
process_document(<<?BSON_EOT, Remainder/binary>>, Acc) ->
    {Acc, Remainder};
process_document(<<?uint8(Type), Binary/binary>>, Acc) ->
    {Key, Value, Remainder} = process_field(Type, Binary),
    process_document(Remainder, Acc#{Key => Value}).

-spec process_array(Binary :: binary(), Acc :: list()) -> {list(), binary()}.
process_array(<<?BSON_EOT, Remainder/binary>>, Acc) ->
    {Acc, Remainder};
process_array(<<?uint8(Type), Binary/binary>>, Acc) ->
    {Key, Value, Remainder} = process_field(Type, Binary),
    Length = erlang:binary_to_integer(Key),
    Length = erlang:length(Acc),
    process_array(Remainder, Acc ++ [Value]).

-spec process_field(Type :: integer(), Binary :: binary()) -> {binary(), term(), binary()}.
process_field(Type, Binary) ->
    {Key, Rest} = process_key(Binary),
    {Value, Remainder} = process_value(Type, Rest),
    {Key, Value, Remainder}.

-spec process_key(Binary :: binary()) -> {binary(), binary()}.
process_key(Binary) ->
    read_terminal(Binary).

-spec process_value(Type :: integer(), Binary :: binary()) -> {term(), binary()}.
process_value(?BSON_INT32, Binary) -> process_int32(Binary);
process_value(?BSON_INT64, Binary) -> process_int64(Binary);
process_value(?BSON_FLOAT64, Binary) -> process_float64(Binary);
process_value(?BSON_BOOLEAN, Binary) -> process_boolean(Binary);
process_value(?BSON_STRING, Binary) -> process_string(Binary);
process_value(?BSON_NULL, Binary) -> process_null(Binary);
process_value(?BSON_BINARY, Binary) -> process_binary(Binary);
process_value(?BSON_DATE, Binary) -> process_date(Binary);
process_value(?BSON_REGEXP, Binary) -> process_regexp(Binary);
process_value(?BSON_TIMESTAMP, Binary) -> process_timestamp(Binary);
process_value(?BSON_JAVASCRIPT, Binary) -> process_javascript(Binary);
process_value(?BSON_OBJECTID, Binary) -> process_objectid(Binary);
process_value(?BSON_DOCUMENT, <<?int32(_), Binary/binary>>) -> process_document(Binary);
process_value(?BSON_ARRAY, <<?int32(_), Binary/binary>>) -> process_array(Binary);
process_value(?MIN_KEY, Binary) -> process_min_key(Binary);
process_value(?MAX_KEY, Binary) -> process_max_key(Binary).

-spec read_terminal(Binary :: binary()) -> {binary(), binary()}.
read_terminal(Binary) ->
    read_terminal(Binary, <<>>).

-spec read_terminal(Binary :: binary(), Acc :: binary()) -> {binary(), binary()}.
read_terminal(<<?BSON_EOT, Remainder/binary>>, Acc) ->
    {Acc, Remainder};
read_terminal(<<Bit:1/binary, Remainder/binary>>, Acc) ->
    read_terminal(Remainder, <<Acc/binary, Bit/binary>>).
