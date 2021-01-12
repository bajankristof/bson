-module(bson_encoder).

-export([
    byte/1,
    int32/1,
    uint32/1,
    int64/1,
    uint64/1,
    long/1,
    double/1,
    boolean/1,
    string/1,
    cstring/1,
    objectid/1,
    datetime/1,
    timestamp/1,
    javascript/1,
    regexp/1,
    binary/1,
    document/1,
    array/1
]).
-export([struct/1]).
-export([void/1]).

-include("./constants.hrl").
-include("./functions.hrl").

%% API functions

-spec byte(Value :: integer()) -> binary().
byte(Value) when ?isbyte(Value) ->
    <<?byte(Value)>>.

-spec int32(Value :: integer()) -> binary().
int32(Value) when ?isint32(Value) ->
    <<?int32(Value)>>.

uint32(Value) when ?isuint32(Value) ->
    <<?uint32(Value)>>.

-spec int64(Value :: integer()) -> binary().
int64(Value) when ?isint64(Value) ->
    <<?int64(Value)>>.

-spec uint64(Value :: integer()) -> binary().
uint64(Value) when ?isuint64(Value) ->
    <<?uint64(Value)>>.

-spec long(Value :: bson:long()) -> binary().
long({long, Value}) when ?isint64(Value) ->
    <<?int64(Value)>>.

-spec double(Value :: float()) -> binary().
double(Value) when erlang:is_float(Value) ->
    <<?double(Value)>>;
double(Value) when erlang:is_integer(Value) ->
    <<?double(Value)>>.

-spec boolean(Value :: boolean()) -> binary().
boolean(true) -> <<?byte(1)>>;
boolean(false) -> <<?byte(0)>>.

-spec string(Value :: binary()) -> binary().
string(Value) when erlang:is_binary(Value) ->
    struct([{int32, erlang:byte_size(Value) + 1}, {cstring, Value}]);
string(Value) when erlang:is_list(Value) ->
    string(erlang:list_to_binary(Value));
string(Value) when erlang:is_atom(Value) ->
    string(erlang:atom_to_binary(Value)).

-spec cstring(Value :: binary()) -> binary().
cstring(Value) when erlang:is_binary(Value) ->
    <<Value/binary, ?byte(0)>>;
cstring(Value) when erlang:is_list(Value) ->
    cstring(erlang:list_to_binary(Value));
cstring(Value) when erlang:is_atom(Value) ->
    cstring(erlang:atom_to_binary(Value)).

-spec objectid(bson:objectid()) -> binary().
objectid({Value}) when erlang:is_binary(Value) ->
    <<Value:12/binary>>.

-spec datetime(bson:datetime()) -> binary().
datetime({Megasecs, Secs, Microsecs}) ->
    <<?int64((Megasecs * 1000000000 + Secs * 1000 + Microsecs div 1000))>>.

-spec timestamp(bson:timestamp()) -> binary().
timestamp({timestamp, Value}) ->
    uint64(Value).

-spec javascript(bson:javascript()) -> binary().
javascript({javascript, Value}) ->
    string(Value).

-spec regexp(bson:regexp()) -> binary().
regexp({regexp, [_, _] = Value}) ->
    erlang:list_to_binary(lists:map(fun cstring/1, Value)).

-spec binary(bson:bin()) -> binary().
binary({binary, Value}) when erlang:is_binary(Value) ->
    <<?int32((erlang:byte_size(Value))), ?byte(?BINARY_SUBTYPE_DEFAULT), Value/binary>>;
binary({function, Value}) when erlang:is_binary(Value) ->
    <<?int32((erlang:byte_size(Value))), ?byte(?BINARY_SUBTYPE_FUNCTION), Value/binary>>;
binary({uuid, Value}) when erlang:is_binary(Value) ->
    <<?int32((erlang:byte_size(Value))), ?byte(?BINARY_SUBTYPE_UUID), Value/binary>>;
binary({md5, Value}) when erlang:is_binary(Value) ->
    <<?int32((erlang:byte_size(Value))), ?byte(?BINARY_SUBTYPE_MD5), Value/binary>>;
binary({'$$', Value}) when erlang:is_binary(Value) ->
    <<?int32((erlang:byte_size(Value))), ?byte(?BINARY_SUBTYPE_USER_DEFINED), Value/binary>>.

-spec document(Document :: map() | list()) -> binary().
document(Document) when erlang:is_map(Document) orelse erlang:is_list(Document) ->
    Payload = elist(document, Document),
    <<?int32((erlang:byte_size(Payload) + 5)), Payload/binary, ?byte(0)>>.

-spec array(Document :: list()) -> binary().
array(Document) when erlang:is_list(Document) ->
    Payload = elist(array, Document),
    <<?int32((erlang:byte_size(Payload) + 5)), Payload/binary, ?byte(0)>>.

-spec struct(Spec :: list({Function :: atom(), Value :: term()})) -> binary().
struct(Spec) ->
    lists:foldl(fun ({Function, Value}, Acc) ->
        <<Acc/binary, (erlang:apply(?MODULE, Function, [Value]))/binary>>
    end, <<>>, Spec).

-spec void(_) -> binary().
void(_) -> <<>>.

%% private functions

-spec elist(Type :: atom(), Document :: map() | list()) -> binary().
elist(document, []) ->
    elist(document, #{});
elist(document, [_|_] = Document) ->
    fold(fun (_, {Key, Value}, Acc) ->
        <<Acc/binary, (elem(Key, Value))/binary>>
    end, Document);
elist(_, Document) ->
    fold(fun (Key, Value, Acc) ->
        <<Acc/binary, (elem(Key, Value))/binary>>
    end, Document).

-spec elem(Key :: binary() | list() | atom(), Value :: term()) -> binary().
elem(Key, Value) ->
    {Type, Function} = whatis(Value),
    struct([{byte, Type}, {cstring, Key}, {Function, Value}]).

-spec whatis(Value :: term()) -> {Type :: integer(), Function :: atom()}.
whatis(null) -> {?NULL, void};
whatis('MIN_KEY') -> {?MIN_KEY, void};
whatis('MAX_KEY') -> {?MAX_KEY, void};
whatis(Value) when ?isint32(Value) -> {?INT32, int32};
whatis(Value) when ?isint64(Value) -> {?INT64, int64};
whatis(Value) when erlang:is_float(Value) -> {?DOUBLE, double};
whatis(Value) when erlang:is_boolean(Value) -> {?BOOLEAN, boolean};
whatis(Value) when erlang:is_binary(Value) -> {?STRING, string};
whatis(Value) when erlang:is_atom(Value) -> {?STRING, string};
whatis({_}) -> {?OBJECTID, objectid};
whatis({_, _, _}) -> {?DATETIME, datetime};
whatis({timestamp, _}) -> {?TIMESTAMP, timestamp};
whatis({javascript, _}) -> {?JAVASCRIPT, javascript};
whatis({regexp, _}) -> {?REGEXP, regexp};
whatis({binary, _}) -> {?BINARY, binary};
whatis({function, _}) -> {?BINARY, binary};
whatis({uuid, _}) -> {?BINARY, binary};
whatis({md5, _}) -> {?BINARY, binary};
whatis({'$$', _}) -> {?BINARY, binary};
whatis({long, _}) -> {?LONG, long};
whatis(#{}) -> {?DOCUMENT, document};
whatis([_|_]) -> {?ARRAY, array};
whatis([]) -> {?ARRAY, array}.

-spec fold(fun(), map() | list()) -> binary().
fold(Function, #{} = Document) ->
    maps:fold(Function, <<>>, Document);
fold(Function, Document) ->
    bson:loop(fun
        ({[], _, Acc}) -> {false, Acc};
        ({[Value|Rest], Key, Acc}) ->
            {true, {Rest, Key + 1, Function(erlang:integer_to_binary(Key), Value, Acc)}}
    end, {Document, 0, <<>>}).
