-module(bson_encoder).

-export([process/1]).

-include_lib("bson/include/bson.hrl").
-include("./bson.hrl").

%% @doc Returns the BSON representation of `Document`.
-spec process(Document :: map()) -> binary().
process(#{} = Document) ->
    process_document(Document).

%% === TYPE ENCODER FUNCTIONS ===

%% @doc Returns the BSON int32 representation of `Int`.
-spec process_int32(Int :: integer()) -> binary().
process_int32(Int) -> <<?int32(Int)>>.

%% @doc Returns the BSON int64 representation of `Int`.
-spec process_int64(Int :: integer()) -> binary().
process_int64(Int) -> <<?int64(Int)>>.

%% @doc Returns the BSON float64 representation of `Float`.
-spec process_float64(Float :: float()) -> binary().
process_float64(Float) -> <<?float64(Float)>>.

%% @doc Returns the BSON boolean representation of `Boolean`.
-spec process_boolean(Boolean :: boolean()) -> binary().
process_boolean(true) -> <<1>>;
process_boolean(false) -> <<0>>.

%% @doc Returns the BSON string representation of `String`.
-spec process_string(binary()) -> binary().
process_string(String) ->
    Size = (erlang:byte_size(String) + 1),
    <<?int32(Size), String/binary, ?BSON_EOT>>.

%% @doc Returns the BSON representation of null.
-spec process_null(null) -> binary().
process_null(null) -> <<>>.

%% @doc Returns the BSON representation of `Binary`.
-spec process_binary(Binary :: bson:bin()) -> binary().
process_binary({Type, String}) ->
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
-spec process_date(Date :: bson:date()) -> binary().
process_date({Megasecs, Secs, Microsecs}) ->
    Millisecs = Megasecs * 1000000000 + Secs * 1000 + Microsecs div 1000,
    <<?int64(Millisecs)>>.

%% @doc Returns the BSON regexp representation of `Regexp`.
-spec process_regexp(Regexp :: bson:regexp()) -> binary().
process_regexp({regexp, Regexp, Opts}) ->
    <<(write_terminal(Regexp))/binary, (write_terminal(Opts))/binary>>.

%% @doc Returns the BSON timestamp representation of `Timestamp`.
-spec process_timestamp(Timestamp :: bson:timestamp()) -> binary().
process_timestamp({timestamp, Timestamp, Id}) ->
    <<?uint32(Timestamp), ?uint32(Id)>>.

%% @doc Returns the BSON javascript representation of `Javascript`.
-spec process_javascript(Javascript :: bson:javascript()) -> binary().
process_javascript({javascript, Code}) ->
    process_string(Code).

%% @doc Returns the BSON object id representation of `Id`.
-spec process_objectid(Id :: bson:objectid()) -> binary().
process_objectid({Id}) -> <<Id:12/binary>>.

%% @doc Returns the BSON min key representation of `MIN_KEY`.
-spec process_min_key(bson:min_key()) -> binary().
process_min_key('MIN_KEY') -> <<>>.

%% @doc Returns the BSON max key representation of `MAX_KEY`.
-spec process_max_key(bson:max_key()) -> binary().
process_max_key('MAX_KEY') -> <<>>.

%% @doc Returns the BSON document or array representation of `Document`.
-spec process_document(Document :: map() | list()) -> binary().
process_document(Document) ->
    Binary = fold_complex(fun (Key, Value, Acc) ->
        <<Acc/binary, (process_field(Key, Value))/binary>>
    end, Document),
    Size = (erlang:byte_size(Binary) + 5),
    <<?int32(Size), Binary/binary, ?BSON_EOT>>.

%% @doc Returns the BSON array representation of `Array`.
-spec process_array(Array :: list()) -> binary().
process_array(Array) ->
    process_document(Array).

%% === UTILITY ENCODER FUNCTIONS ===

-spec process_field(term(), term()) -> binary().
process_field(Key, Term) ->
    {Type, Value} = process_value(Term),
    <<?uint8(Type), (process_key(Key))/binary, Value/binary>>.

-spec process_key(term()) -> binary().
process_key(Key) when erlang:is_binary(Key) ->
    write_terminal(Key);
process_key(Key) when erlang:is_atom(Key) ->
    process_key(erlang:atom_to_binary(Key, utf8));
process_key(Key) when erlang:is_integer(Key) ->
    process_key(erlang:integer_to_binary(Key)).

-spec process_value(term()) -> {integer(), binary()}.
process_value(null) -> {?BSON_NULL, process_null(null)};
process_value('MIN_KEY') -> {?MIN_KEY, process_min_key('MIN_KEY')};
process_value('MAX_KEY') -> {?MAX_KEY, process_max_key('MAX_KEY')};
process_value({_} = Id) -> {?BSON_OBJECTID, process_objectid(Id)};
process_value({regexp, _, _} = Regexp) -> {?BSON_REGEXP, process_regexp(Regexp)};
process_value({timestamp, _, _} = Timestamp) -> {?BSON_TIMESTAMP, process_timestamp(Timestamp)};
process_value({javascript, _} = Javascript) -> {?BSON_JAVASCRIPT, process_javascript(Javascript)};
process_value({binary, _} = Binary) -> {?BSON_BINARY, process_binary(Binary)};
process_value({function, _} = Binary) -> {?BSON_BINARY, process_binary(Binary)};
process_value({uuid, _} = Binary) -> {?BSON_BINARY, process_binary(Binary)};
process_value({md5, _} = Binary) -> {?BSON_BINARY, process_binary(Binary)};
process_value({'$$', _} = Binary) -> {?BSON_BINARY, process_binary(Binary)};
process_value({_, _, _} = Date) -> {?BSON_DATE, process_date(Date)};
process_value(Int) when ?isint32(Int) -> {?BSON_INT32, process_int32(Int)};
process_value(Int) when ?isint64(Int) -> {?BSON_INT64, process_int64(Int)};
process_value(Float) when erlang:is_float(Float) -> {?BSON_FLOAT64, process_float64(Float)};
process_value(Boolean) when erlang:is_boolean(Boolean) -> {?BSON_BOOLEAN, process_boolean(Boolean)};
process_value(String) when erlang:is_binary(String) -> {?BSON_STRING, process_string(String)};
process_value(Document) when erlang:is_map(Document) -> {?BSON_DOCUMENT, process_document(Document)};
process_value(Array) when erlang:is_list(Array) -> {?BSON_ARRAY, process_array(Array)};
process_value(Atom) when erlang:is_atom(Atom) -> process_value(erlang:atom_to_binary(Atom, utf8)).

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
