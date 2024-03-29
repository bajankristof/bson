-module(decode_test).

-include_lib("eunit/include/eunit.hrl").
-include("bson.hrl").

-define(TESTS, [
    %% {}
    {#{}, <<5,0,0,0,0>>},
    %% { key: Int32(1) }
    {#{<<"key">> => 1}, <<14,0,0,0,16,107,101,121,0,1,0,0,0,0>>},
    %% { key: Int64(1) }
    {#{<<"key">> => 1}, <<18,0,0,0,18,107,101,121,0,1,0,0,0,0,0,0,0,0>>},
    %% { key: Float64(1.1) }
    {#{<<"key">> => 1.1}, <<18,0,0,0,1,107,101,121,0,154,153,153,153,153,153,241,63,0>>},
    %% { key: true }
    {#{<<"key">> => true}, <<11,0,0,0,8,107,101,121,0,1,0>>},
    %% { key: false }
    {#{<<"key">> => false}, <<11,0,0,0,8,107,101,121,0,0,0>>},
    %% { key: null }
    {#{<<"key">> => null}, <<10,0,0,0,10,107,101,121,0,0>>},
    %% { key: String('value') }
    {#{<<"key">> => <<"value">>}, <<20,0,0,0,2,107,101,121,0,6,0,0,0,118,97,108,117,101,0,0>>},
    %% { root: { key: [] } }
    {#{<<"root">> => #{<<"key">> => []}}, <<26,0,0,0,3,114,111,111,116,0,15,0,0,0,4,107,101,121,0,5,0,0,0,0,0,0>>},
    %% { key: [null, true, String('2'), Int32(3)] }
    {#{<<"key">> => [null, true, <<"2">>, 3]}, <<38,0,0,0,4,107,101,121,0,28,0,0,0,10,48,0,8,49,0,1,2,50,0,2,0,0,0,50,0,16,51,0,3,0,0,0,0,0>>},
    %% { key: Binary() }
    {#{<<"key">> => #'bson.binary'{type = binary, value = <<"yeahboi">>}}, <<22,0,0,0,5,107,101,121,0,7,0,0,0,0,121,101,97,104,98,111,105,0>>},
    %% { key: Date() }
    {#{<<"key">> => {3376,684800,0}}, <<18,0,0,0,9,107,101,121,0,0,24,25,50,18,3,0,0,0>>},
    %% { key: RegExp() }
    {#{<<"key">> => #'bson.regexp'{value = [<<"^.*$">>, <<"is">>]}}, <<18,0,0,0,11,107,101,121,0,94,46,42,36,0,105,115,0,0>>},
    %% { key: Timestamp() }
    {#{<<"key">> => #'bson.timestamp'{value = 7671652096}}, <<18,0,0,0,17,107,101,121,0,0,31,68,201,1,0,0,0,0>>},
    %% { key: JavaScript() }
    {#{<<"key">> => #'bson.javascript'{value = <<"() => 1">>}}, <<22,0,0,0,13,107,101,121,0,8,0,0,0,40,41,32,61,62,32,49,0,0>>},
    %% { key: ObjectId() }
    {#{<<"_id">> => #'bson.objectid'{value = <<95,220,150,226,21,1,11,30,59,151,141,20>>}}, <<22,0,0,0,7,95,105,100,0,95,220,150,226,21,1,11,30,59,151,141,20,0>>},
    %% { key: MinKey() }
    {#{<<"key">> => 'MIN_KEY'}, <<10,0,0,0,255,107,101,121,0,0>>},
    %% { key: MaxKey() }
    {#{<<"key">> => 'MAX_KEY'}, <<10,0,0,0,127,107,101,121,0,0>>}
]).

decode_test_() ->
    lists:map(fun ({Expected, Binary}) ->
        ?_assertEqual({Expected, <<>>}, bson:decode(Binary))
    end, ?TESTS).
