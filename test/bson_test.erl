-module(bson_test).

-include_lib("eunit/include/eunit.hrl").

binary_to_objectid_test_() ->
    [?_assertEqual({<<95,253,186,105,173,72,218,42,117,172,2,15>>}, bson:binary_to_objectid(<<"5ffdba69ad48da2a75ac020f">>))].

objectid_to_binary_test_() ->
    [?_assertEqual(<<"5ffdba69ad48da2a75ac020f">>, bson:objectid_to_binary({<<95,253,186,105,173,72,218,42,117,172,2,15>>}))].
