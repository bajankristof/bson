-module(bson_encoder).

-export([encode/1]).

-include_lib("bson/include/bson.hrl").
-include("./bson.hrl").

%% @doc Returns the BSON representation of `Document`.
-spec encode(Document :: map()) -> binary().
encode(#{} = Document) ->
    encode_document(Document).

%% === TYPE ENCODER FUNCTIONS ===

%% @doc Returns the BSON int32 representation of `Int`.
-spec encode_int32(Int :: integer()) -> binary().
encode_int32(Int) -> <<?int32(Int)>>.

%% @doc Returns the BSON int64 representation of `Int`.
-spec encode_int64(Int :: integer()) -> binary().
encode_int64(Int) -> <<?int64(Int)>>.

%% @doc Returns the BSON float64 representation of `Float`.
-spec encode_float64(Float :: float()) -> binary().
encode_float64(Float) -> <<?float64(Float)>>.

%% @doc Returns the BSON boolean representation of `Boolean`.
-spec encode_boolean(Boolean :: boolean()) -> binary().
encode_boolean(true) -> <<1>>;
encode_boolean(false) -> <<0>>.

%% @doc Returns the BSON string representation of `String`.
-spec encode_string(binary()) -> binary().
encode_string(String) ->
    Size = (erlang:byte_size(String) + 1),
    <<?int32(Size), String/binary, ?BSON_EOT>>.

%% @doc Returns the BSON representation of null.
-spec encode_null(null) -> binary().
encode_null(null) -> <<>>.

%% @doc Returns the BSON representation of `Binary`.
-spec encode_binary(Binary :: bson:bin()) -> binary().
encode_binary({Type, String}) ->
    Size = erlang:byte_size(String),
    Tag = case Type of
        binary -> ?BSON_SUBTYPE_BINARY;
        function -> ?BSON_SUBTYPE_FUNCTION;
        uuid -> ?BSON_SUBTYPE_UUID;
        md5 -> ?BSON_SUBTYPE_MD5;
        '$$' -> ?BSON_SUBTYPE_USER_DEFINED
    end,
    <<?int32(Size), ?uint8(Tag), String/binary>>.

%% @doc Returns the BSON UTC date representation of `Date`.
-spec encode_date(Date :: bson:date()) -> binary().
encode_date({Megasecs, Secs, Microsecs}) ->
    Millisecs = Megasecs * 1000000000 + Secs * 1000 + Microsecs div 1000,
    <<?int64(Millisecs)>>.

%% @doc Returns the BSON regexp representation of `Regexp`.
-spec encode_regexp(Regexp :: bson:regexp()) -> binary().
encode_regexp({regexp, Regexp, Opts}) ->
    <<(write_terminal(Regexp))/binary, (write_terminal(Opts))/binary>>.

%% @doc Returns the BSON timestamp representation of `Timestamp`.
-spec encode_timestamp(Timestamp :: bson:timestamp()) -> binary().
encode_timestamp({timestamp, Timestamp, Id}) ->
    <<?uint32(Timestamp), ?uint32(Id)>>.

%% @doc Returns the BSON javascript representation of `Javascript`.
-spec encode_javascript(Javascript :: bson:javascript()) -> binary().
encode_javascript({javascript, Code}) ->
    encode_string(Code).

%% @doc Returns the BSON object id representation of `Id`.
-spec encode_objectid(Id :: bson:objectid()) -> binary().
encode_objectid({Id}) -> <<Id:12/binary>>.

%% @doc Returns the BSON min key representation of `MIN_KEY`.
-spec encode_min_key(bson:min_key()) -> binary().
encode_min_key('MIN_KEY') -> <<>>.

%% @doc Returns the BSON max key representation of `MAX_KEY`.
-spec encode_max_key(bson:max_key()) -> binary().
encode_max_key('MAX_KEY') -> <<>>.

%% @doc Returns the BSON document or array representation of `Document`.
-spec encode_document(Document :: map() | list()) -> binary().
encode_document(Document) ->
    Binary = fold_complex(fun (Key, Value, Acc) ->
        <<Acc/binary, (encode_field(Key, Value))/binary>>
    end, Document),
    Size = (erlang:byte_size(Binary) + 5),
    <<?int32(Size), Binary/binary, ?BSON_EOT>>.

%% @doc Returns the BSON array representation of `Array`.
-spec encode_array(Array :: list()) -> binary().
encode_array(Array) ->
    encode_document(Array).

%% === UTILITY ENCODER FUNCTIONS ===

-spec encode_field(term(), term()) -> binary().
encode_field(Key, Term) ->
    {Type, Value} = encode_value(Term),
    <<?uint8(Type), (encode_key(Key))/binary, Value/binary>>.

-spec encode_key(term()) -> binary().
encode_key(Key) when erlang:is_binary(Key) ->
    write_terminal(Key);
encode_key(Key) when erlang:is_atom(Key) ->
    encode_key(erlang:atom_to_binary(Key, utf8));
encode_key(Key) when erlang:is_integer(Key) ->
    encode_key(erlang:integer_to_binary(Key)).

-spec encode_value(term()) -> {integer(), binary()}.
encode_value(null) -> {?BSON_NULL, encode_null(null)};
encode_value('MIN_KEY') -> {?MIN_KEY, encode_min_key('MIN_KEY')};
encode_value('MAX_KEY') -> {?MAX_KEY, encode_max_key('MAX_KEY')};
encode_value({_} = Id) -> {?BSON_OBJECTID, encode_objectid(Id)};
encode_value({regexp, _, _} = Regexp) -> {?BSON_REGEXP, encode_regexp(Regexp)};
encode_value({timestamp, _, _} = Timestamp) -> {?BSON_TIMESTAMP, encode_timestamp(Timestamp)};
encode_value({javascript, _} = Javascript) -> {?BSON_JAVASCRIPT, encode_javascript(Javascript)};
encode_value({binary, _} = Binary) -> {?BSON_BINARY, encode_binary(Binary)};
encode_value({function, _} = Binary) -> {?BSON_BINARY, encode_binary(Binary)};
encode_value({uuid, _} = Binary) -> {?BSON_BINARY, encode_binary(Binary)};
encode_value({md5, _} = Binary) -> {?BSON_BINARY, encode_binary(Binary)};
encode_value({'$$', _} = Binary) -> {?BSON_BINARY, encode_binary(Binary)};
encode_value({_, _, _} = Date) -> {?BSON_DATE, encode_date(Date)};
encode_value(Int) when ?isint32(Int) -> {?BSON_INT32, encode_int32(Int)};
encode_value(Int) when ?isint64(Int) -> {?BSON_INT64, encode_int64(Int)};
encode_value(Float) when erlang:is_float(Float) -> {?BSON_FLOAT64, encode_float64(Float)};
encode_value(Boolean) when erlang:is_boolean(Boolean) -> {?BSON_BOOLEAN, encode_boolean(Boolean)};
encode_value(String) when erlang:is_binary(String) -> {?BSON_STRING, encode_string(String)};
encode_value(Document) when erlang:is_map(Document) -> {?BSON_DOCUMENT, encode_document(Document)};
encode_value(Array) when erlang:is_list(Array) -> {?BSON_ARRAY, encode_array(Array)};
encode_value(Atom) when erlang:is_atom(Atom) -> encode_value(erlang:atom_to_binary(Atom, utf8)).

-spec write_terminal(binary()) -> binary().
write_terminal(Binary) ->
    <<Binary/binary, ?BSON_EOT>>.

-spec fold_complex(fun(), term()) -> binary().
fold_complex(Function, #{} = Document) ->
    maps:fold(Function, <<>>, Document);
fold_complex(Function, Array) ->
    fold_complex(Function, Array, 0, <<>>).

-spec fold_complex(fun(), list(), integer(), binary()) -> binary().
fold_complex(_, [], _, Acc) -> Acc;
fold_complex(Function, [Head|Rest], Index, Acc) ->
    fold_complex(Function, Rest, Index + 1, Function(Index, Head, Acc)).
