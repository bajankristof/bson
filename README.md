
bson
===

BSON implementation in Erlang.

[Read the docs!](https://github.com/bajankristof/bson/blob/main/doc/bson.md)

Build
---

    $ rebar3 compile

Getting started
---

```erlang
-include_lib("bson/include/bson.hrl").
```

### `bson:encode`
```erlang
bson:decode(<<22,0,0,0,7,95,105,100,0,95,220,150,226,21,1,11,30,59,151,141,20,0>>).
%% #{<<"_id">> => #'bson.objectid'{value = <<95,220,150,226,21,1,11,30,59,151,141,20>>}}
```

### `bson:decode`
```erlang
bson:encode(#{<<"_id">> => bson:binary_to_objectid(<<"5fdc96e215010b1e3b978d14">>)}).
%% {<<22,0,0,0,7,95,105,100,0,95,220,150,226,21,1,11,30,59,151,141,20,0>>, <<>>}
```

### `bson:construct`
```erlang
bson:construct([{int32, 1}, {document, #{}}]).
%% <<1,0,0,0,5,0,0,0,0>>
```

### `bson:destruct`
```erlang
bson:destruct([int32, document], <<1,0,0,0,5,0,0,0,0>>).
%% {[1, #{}], <<>>}
```
