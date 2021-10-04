-module(deeperl_usage_tests).
-include_lib("eunit/include/eunit.hrl").

request_test() ->
    ?assertEqual(
        {post, {
            "/v2/usage",
            [],
            "application/x-www-form-urlencoded; charset=utf-8",
            ""}
        },
        deeperl_usage:request({})
    ).

response_test() ->
    ?assertEqual(
        {ok, {123, 456}},
        deeperl_usage:response(<<"{\"character_count\":123, \"character_limit\":456}">>)
    ).
