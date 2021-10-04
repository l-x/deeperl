-module(deeperl_target_languages_tests).
-include_lib("eunit/include/eunit.hrl").

request_test() ->
    ?assertEqual(
        {post, {
            "/v2/languages",
            [],
            "application/x-www-form-urlencoded; charset=utf-8",
            "type=target"}
        },
        deeperl_target_languages:request({})
    ).

response_test() ->
    ApiResponse = jiffy:encode([
        #{language => <<"lang1">>, name => <<"name1">>, supports_formality => true},
        #{language => <<"lang2">>, name => <<"name2">>, supports_formality => false},
        #{language => <<"lang3">>, name => <<"name3">>, supports_formality => false}
    ]),

    ?assertEqual(
        {ok, [
            {"lang1", "name1", true},
            {"lang2", "name2", false},
            {"lang3", "name3", false}
        ]},
        deeperl_target_languages:response(ApiResponse)
    ).
