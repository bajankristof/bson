-define(byte(Value), Value:8).
-define(int32(Value), Value:32/little-signed).
-define(uint32(Value), Value:32/little).
-define(int64(Value), Value:64/little-signed).
-define(uint64(Value), Value:64/little).
-define(double(Value), Value:64/little-float).
-define(string(Size, Value), ?int32(Size), Value/binary).
-define(cstring(Value), Value/binary, ?byte(0)).

-define(isbyte(Value), erlang:is_integer(Value)).
-define(isint32(Value), erlang:is_integer(Value) andalso -16#80000000 =< Value andalso Value =< 16#7fffffff).
-define(isuint32(Value), erlang:is_integer(Value) andalso 0 =< Value andalso Value =< 16#ffffffff).
-define(isint64(Value), erlang:is_integer(Value) andalso -16#8000000000000000 =< Value andalso Value =< 16#7fffffffffffffff).
-define(isuint64(Value), erlang:is_integer(Value) andalso 0 =< Value andalso Value =< 16#ffffffffffffffff).
-define(isdouble(Value), erlang:is_float(Value)).
-define(isboolean(Value), erlang:is_boolean(Value)).
-define(isstring(Value), erlang:is_binary(Value) orelse erlang:is_atom(Value)).