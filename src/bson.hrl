-define(uint8(Var), Var:8/little).
-define(int8(Var), Var:8/little-signed).
-define(uint32(Var), Var:32/little).
-define(int32(Var), Var:32/little-signed).
-define(uint64(Var), Var:64/little).
-define(int64(Var), Var:64/little-signed).
-define(float64(Var), Var:64/little-float).

-define(isint32(Var), erlang:is_integer(Var) andalso -16#80000000 =< Var andalso Var =< 16#7fffffff).
-define(isint64(Var), erlang:is_integer(Var) andalso -16#8000000000000000 =< Var andalso Var =< 16#7fffffffffffffff).
