-module(deeperl_source_languages_tests).
-include_lib("eunit/include/eunit.hrl").

request_test() ->
    ?assertEqual(
        {post, {
            "/v2/languages",
            [],
            "application/x-www-form-urlencoded; charset=utf-8",
            "type=source"}
        },
        deeperl_source_languages:request({})
    ).

response_test() ->
    ApiResponse = jiffy:encode([
        #{language => <<"lang1">>, name => <<"name1">>},
        #{language => <<"lang2">>, name => <<"name2">>},
        #{language => <<"lang3">>, name => <<"name3">>}
    ]),

    ?assertEqual(
        {ok, [
            {"lang1", "name1"},
            {"lang2", "name2"},
            {"lang3", "name3"}
        ]},
        deeperl_source_languages:response(ApiResponse)
    ).
