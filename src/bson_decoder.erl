-module(bson_decoder).

-export([
    byte/1,
    int32/1,
    uint32/1,
    int64/1,
    uint64/1,
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
-export([struct/2]).

-include("bson.hrl").
-include("./constants.hrl").
-include("./functions.hrl").

-spec byte(Payload :: binary()) -> {integer(), binary()}.
byte(<<?byte(Value), Remainder/binary>>) ->
    {Value, Remainder}.

-spec int32(Payload :: binary()) -> {integer(), binary()}.
int32(<<?int32(Value), Remainder/binary>>) ->
    {Value, Remainder}.

-spec uint32(Payload :: binary()) -> {integer(), binary()}.
uint32(<<?uint32(Value), Remainder/binary>>) ->
    {Value, Remainder}.

-spec int64(Payload :: binary()) -> {integer(), binary()}.
int64(<<?int64(Value), Remainder/binary>>) ->
    {Value, Remainder}.

-spec uint64(Payload :: binary()) -> {integer(), binary()}.
uint64(<<?uint64(Value), Remainder/binary>>) ->
    {Value, Remainder}.

-spec double(Payload :: binary()) -> {float(), binary()}.
double(<<?double(Value), Remainder/binary>>) ->
    {Value, Remainder}.

-spec boolean(Payload :: binary()) -> {boolean(), binary()}.
boolean(<<?byte(Value), Remainder/binary>>) ->
    {Value =:= 1, Remainder}.

-spec string(Payload :: binary()) -> {binary(), binary()}.
string(<<?int32(Size), Value:(Size - 1)/binary, ?byte(0), Remainder/binary>>) ->
    {Value, Remainder}.

-spec cstring(Payload :: binary()) -> {binary(), binary()}.
cstring(<<Payload/binary>>) ->
    bson:loop(fun
        ({Acc, <<?byte(0), Remainder/binary>>}) ->
            {false, {Acc, Remainder}};
        ({Acc, <<Char:1/binary, Remainder/binary>>}) ->
            {true, {<<Acc/binary, Char/binary>>, Remainder}}
    end, {<<>>, Payload}).

-spec objectid(Payload :: binary()) -> {bson:objectid(), binary()}.
objectid(<<Value:12/binary, Remainder/binary>>) ->
    {#'bson.objectid'{value = Value}, Remainder}.

-spec datetime(Payload :: binary()) -> {bson:datetime(), binary()}.
datetime(<<?int64(Value), Remainder/binary>>) ->
    {{Value div 1000000000, (Value div 1000) rem 1000000, (Value * 1000) rem 1000000}, Remainder}.

-spec timestamp(Payload :: binary()) -> {bson:timestamp(), binary()}.
timestamp(<<?uint64(Value), Remainder/binary>>) ->
    {#'bson.timestamp'{value = Value}, Remainder}.

-spec javascript(Payload :: binary()) -> {bson:javascript(), binary()}.
javascript(<<Payload/binary>>) ->
    {Value, Remainder} = string(Payload),
    {#'bson.javascript'{value = Value}, Remainder}.

-spec regexp(Payload :: binary()) -> {bson:regexp(), binary()}.
regexp(<<Payload/binary>>) ->
    {Value, Remainder} = struct([cstring, cstring], Payload),
    {#'bson.regexp'{value = Value}, Remainder}.

-spec binary(Payload :: binary()) -> {bson:binary(), binary()}.
binary(<<?int32(Size), ?byte(?BINARY_SUBTYPE_DEFAULT), Value:Size/binary, Remainder/binary>>) ->
    {#'bson.binary'{type = binary, value = Value}, Remainder};
binary(<<?int32(Size), ?byte(?BINARY_SUBTYPE_FUNCTION), Value:Size/binary, Remainder/binary>>) ->
    {#'bson.binary'{type = function, value = Value}, Remainder};
binary(<<?int32(Size), ?byte(?BINARY_SUBTYPE_UUID), Value:Size/binary, Remainder/binary>>) ->
    {#'bson.binary'{type = uuid, value = Value}, Remainder};
binary(<<?int32(Size), ?byte(?BINARY_SUBTYPE_MD5), Value:Size/binary, Remainder/binary>>) ->
    {#'bson.binary'{type = md5, value = Value}, Remainder};
binary(<<?int32(Size), ?byte(?BINARY_SUBTYPE_USER_DEFINED), Value:Size/binary, Remainder/binary>>) ->
    {#'bson.binary'{type = '$$', value = Value}, Remainder}.

-spec document(Payload :: binary()) -> {map(), binary()}.
document(<<?int32(Size), Payload:(Size - 5)/binary, ?byte(0), Remainder/binary>>) ->
    {elist(Payload, #{}), Remainder}.

-spec array(Payload :: binary()) -> {list(), binary()}.
array(<<?int32(Size), Payload:(Size - 5)/binary, ?byte(0), Remainder/binary>>) ->
    {lists:reverse(elist(Payload, [])), Remainder}.

-spec struct(Spec :: list(Function :: atom()), Payload :: binary()) -> {list(), binary()}.
struct(Spec, <<Payload/binary>>) ->
    bson:loop(fun
        ({[], {Acc, Remainder}}) ->
            {false, {lists:reverse(Acc), Remainder}};
        ({[Function | Rest], {Acc, Part}}) ->
            {Value, Remainder} = ?MODULE:Function(Part),
            {true, {Rest, {[Value | Acc], Remainder}}}
    end, {Spec, {[], Payload}}).

%% private functions

-spec elist(Payload :: binary(), Acc0 :: map() | list()) -> map() | list().
elist(<<>>, Acc) -> Acc;
elist(<<?byte(Type), Payload/binary>>, Acc) ->
    case whatis(Type) of
        {void, Value} ->
            {Key, Remainder} = cstring(Payload),
            elist(Remainder, put(Key, Value, Acc));
        Function ->
            {[Key, Value], Remainder} = struct([cstring, Function], Payload),
            elist(Remainder, put(Key, Value, Acc))
    end.

-spec whatis(Type :: integer()) -> atom() | {void, term()}.
whatis(?NULL) -> {void, null};
whatis(?MIN_KEY) -> {void, 'MIN_KEY'};
whatis(?MAX_KEY) -> {void, 'MAX_KEY'};
whatis(?INT32) -> int32;
whatis(?INT64) -> int64;
whatis(?DOUBLE) -> double;
whatis(?BOOLEAN) -> boolean;
whatis(?STRING) -> string;
whatis(?OBJECTID) -> objectid;
whatis(?DATETIME) -> datetime;
whatis(?TIMESTAMP) -> timestamp;
whatis(?JAVASCRIPT) -> javascript;
whatis(?REGEXP) -> regexp;
whatis(?BINARY) -> binary;
whatis(?DOCUMENT) -> document;
whatis(?ARRAY) -> array.

-spec put(Key :: binary(), Value :: term(), Acc :: map() | list()) -> map() | list().
put(Key, Value, #{} = Acc) -> Acc#{Key => Value};
put(_, Value, Acc) -> [Value | Acc].
